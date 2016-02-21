package jcl.compiler.functions;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.CompilerVariables;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.struct.specialoperator.JavaMethodCallStruct;
import jcl.compiler.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.compiler.struct.specialoperator.SetqStruct;
import jcl.compiler.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.compiler.struct.specialoperator.SymbolFunctionCallStruct;
import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.java.JavaMethodStruct;
import jcl.java.JavaNameStruct;
import jcl.java.JavaObjectStruct;
import jcl.java.functions.JInvoke;
import jcl.java.functions.JMethod;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class EvalFunction extends FunctionStruct {

	public static final SymbolStruct EVAL = GlobalPackageStruct.COMMON_LISP.intern("EVAL").getSymbol();

	@Autowired
	private CompileForm compileForm;

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private JMethod jMethod;

	@Autowired
	private JInvoke jInvoke;

	@Autowired
	private Printer printer;

	private EvalFunction() {
		super("Evaluates form in the current dynamic environment and the null lexical environment.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		EVAL.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(EVAL);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FORM").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return eval(lispStructs[0]);
	}

	public LispStruct eval(final LispStruct originalExp) {

		final Environment nullEnvironment = Environment.NULL;
		return eval(originalExp, nullEnvironment);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	public LispStruct eval(final LispStruct originalExp, final Environment environment) {

		final BooleanStruct oldCompileTopLevel = CompilerVariables.COMPILE_TOP_LEVEL.getVariableValue();
		CompilerVariables.COMPILE_TOP_LEVEL.setValue(NILStruct.INSTANCE);

		LispStruct exp;
		try {
			exp = formAnalyzer.analyze(originalExp, environment);
		} finally {
			CompilerVariables.COMPILE_TOP_LEVEL.setValue(oldCompileTopLevel);
		}

		if (exp instanceof SymbolStruct) {
			final SymbolStruct symbol = (SymbolStruct) exp;
			return symbol.getValue();
		}

		if (exp instanceof QuoteStruct) {
			final QuoteStruct quote = (QuoteStruct) exp;
			return quote.getObject();
		}

		if (exp instanceof SetqStruct) {
			final SetqStruct setq = (SetqStruct) exp;
			final List<SetqStruct.SetqPair> setqPairs = setq.getSetqPairs();

			LispStruct finalForm = NullStruct.INSTANCE;

			for (final SetqStruct.SetqPair setqPair : setqPairs) {
				final SymbolStruct var = setqPair.getVar();
				final LispStruct form = setqPair.getForm();
				final LispStruct evaluatedForm = eval(form);

				var.setValue(evaluatedForm);

				finalForm = evaluatedForm;
			}
			return finalForm;
		}

		if (exp instanceof PrognStruct) {
			final PrognStruct progn = (PrognStruct) exp;
			final List<LispStruct> forms = progn.getForms();

			LispStruct finalForm = NullStruct.INSTANCE;

			for (final LispStruct form : forms) {
				finalForm = eval(form);
			}
			return finalForm;
		}

		if (exp instanceof SymbolCompilerFunctionStruct) {
			final SymbolCompilerFunctionStruct symbolCompilerFunction = (SymbolCompilerFunctionStruct) exp;
			final SymbolStruct functionSymbol = symbolCompilerFunction.getFunctionSymbol();
			return functionSymbol.getFunction();
		}

		if (exp instanceof LambdaCompilerFunctionStruct) {
			final LambdaCompilerFunctionStruct lambdaCompilerFunction = (LambdaCompilerFunctionStruct) exp;
			final LambdaStruct lambda = lambdaCompilerFunction.getLambdaStruct();

			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel, lambda);
			return function.apply();
		}

		if (exp instanceof SymbolFunctionCallStruct) {
			final SymbolFunctionCallStruct functionCall = (SymbolFunctionCallStruct) exp;
			final SymbolCompilerFunctionStruct symbolCompilerFunction = functionCall.getSymbolCompilerFunction();
			final SymbolStruct functionSymbol = symbolCompilerFunction.getFunctionSymbol();

			final FunctionStruct function = functionSymbol.getFunction();

			final List<LispStruct> arguments = functionCall.getArguments();
			final List<LispStruct> evaluatedArguments = new ArrayList<>(arguments.size());
			for (final LispStruct argument : arguments) {
				final LispStruct evaluatedArgument = eval(argument);
				evaluatedArguments.add(evaluatedArgument);
			}

			final LispStruct[] args = new LispStruct[evaluatedArguments.size()];
			evaluatedArguments.toArray(args);

			return function.apply(args);
		}

		if (exp instanceof JavaMethodCallStruct) {
			final JavaMethodCallStruct javaMethodCall = (JavaMethodCallStruct) exp;
			final JavaNameStruct methodName = javaMethodCall.getMethodName();

			final LispStruct javaObject = javaMethodCall.getJavaObject();
			final LispStruct evaluatedJavaObject = eval(javaObject);
			if (!(evaluatedJavaObject instanceof JavaObjectStruct)) {
				final String printedObject = printer.print(evaluatedJavaObject);
				throw new ProgramErrorException("EVAL: Second argument to Java method call must be a Java object. Got: " + printedObject);
			}

			final JavaObjectStruct javaObjectStruct = (JavaObjectStruct) evaluatedJavaObject;
			final Object actualJavaObject = javaObjectStruct.getJavaObject();

			final List<LispStruct> arguments = javaMethodCall.getArguments();

			final LispStruct[] methodEvaluatedArgs = new LispStruct[arguments.size()];
			final Class<?>[] methodParamTypes = new Class[arguments.size()];
			for (int i = 0; i < arguments.size(); i++) {
				final LispStruct currentArg = arguments.get(i);
				final LispStruct evaluatedArgument = eval(currentArg);
				methodEvaluatedArgs[i] = evaluatedArgument;
				// TODO: can we dynamically determine the types???
				methodParamTypes[i] = Object.class;
			}

			final JavaMethodStruct javaMethodStruct = jMethod.jMethod(methodName.getJavaName(), actualJavaObject.getClass(), methodParamTypes);
			final Method javaMethod = javaMethodStruct.getJavaMethod();

			return jInvoke.jInvoke(javaMethod, actualJavaObject, methodEvaluatedArgs);
		}

		if (exp instanceof LambdaFunctionCallStruct) {
			final LambdaFunctionCallStruct lambdaFunctionCall = (LambdaFunctionCallStruct) exp;
			final LambdaCompilerFunctionStruct lambdaCompilerFunction = lambdaFunctionCall.getLambdaCompilerFunction();
			final LambdaStruct lambda = lambdaCompilerFunction.getLambdaStruct();

			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel, lambda);

			final List<LispStruct> arguments = lambdaFunctionCall.getArguments();
			final List<LispStruct> evaluatedArguments = new ArrayList<>(arguments.size());
			for (final LispStruct argument : arguments) {
				final LispStruct evaluatedArgument = eval(argument);
				evaluatedArguments.add(evaluatedArgument);
			}

			final LispStruct[] args = new LispStruct[evaluatedArguments.size()];
			evaluatedArguments.toArray(args);

			// NOTE: This cast should be safe since we're compiling a lambda form. If it doesn't cast, we have a bigger problem somewhere.
			final FunctionStruct compiledLambda = (FunctionStruct) function.apply();
			return compiledLambda.apply(args);
		}

		if (exp instanceof CompilerSpecialOperatorStruct) {
			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel, (CompilerSpecialOperatorStruct) exp);
			return function.apply();
		}

		return exp;
	}

	private FunctionStruct getCompiledExpression(final BooleanStruct oldCompileTopLevel, final CompilerSpecialOperatorStruct exp) {
		CompilerVariables.COMPILE_TOP_LEVEL.setValue(NILStruct.INSTANCE);

		final BooleanStruct oldConvertingForInterpreter = CompilerVariables.CONVERTING_FOR_INTERPRETER.getVariableValue();
		CompilerVariables.CONVERTING_FOR_INTERPRETER.setValue(TStruct.INSTANCE);

		final FunctionStruct function;
		try {
			final CompileResult compileResult = compileForm.compile(exp);
			function = compileResult.getFunction();
		} finally {
			CompilerVariables.CONVERTING_FOR_INTERPRETER.setValue(oldConvertingForInterpreter);
			CompilerVariables.COMPILE_TOP_LEVEL.setValue(oldCompileTopLevel);
		}
		return function;
	}
}
