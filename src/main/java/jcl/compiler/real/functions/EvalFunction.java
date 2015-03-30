package jcl.compiler.real.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.FunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.compiler.real.struct.specialoperator.SymbolCompilerFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class EvalFunction extends FunctionStruct {

	public static final SymbolStruct<?> EVAL = new SymbolStruct<>("EVAL", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 6775277576397622716L;

	@Autowired
	private CompileForm compileForm;

	@Autowired
	private FormAnalyzer formAnalyzer;

	private EvalFunction() {
		super("Evaluates form in the current dynamic environment and the null lexical environment.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		EVAL.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = new SymbolStruct<>("FORM", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation listArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(listArgSymbol, listArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return eval(lispStructs[0]);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	public LispStruct eval(final LispStruct originalExp) {

		final Environment nullEnvironment = Environment.NULL;

		final BooleanStruct oldCompileTopLevel = CompilerVariables.COMPILE_TOP_LEVEL.getValue();
		CompilerVariables.COMPILE_TOP_LEVEL.setValue(NILStruct.INSTANCE);

		LispStruct exp;
		try {
			exp = formAnalyzer.analyze(originalExp, nullEnvironment);
		} finally {
			CompilerVariables.COMPILE_TOP_LEVEL.setValue(oldCompileTopLevel);
		}

		if (exp instanceof SymbolStruct) {
			final SymbolStruct<?> symbol = (SymbolStruct) exp;
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
			final SymbolStruct<?> functionSymbol = symbolCompilerFunction.getFunctionSymbol();
			return functionSymbol.getFunction();
		}

		if (exp instanceof LambdaCompilerFunctionStruct) {
			final LambdaCompilerFunctionStruct lambdaCompilerFunction = (LambdaCompilerFunctionStruct) exp;
			final CompilerSpecialOperatorStruct lambda = lambdaCompilerFunction.getLambdaStruct();

			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel, lambda);
			return function.apply();
		}

		if (exp instanceof FunctionCallStruct) {
			final FunctionCallStruct functionCall = (FunctionCallStruct) exp;
			final SymbolStruct<?> functionSymbol = functionCall.getFunctionSymbol();

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

		if (exp instanceof LambdaFunctionCallStruct) {
			final LambdaFunctionCallStruct lambdaFunctionCall = (LambdaFunctionCallStruct) exp;
			final CompilerSpecialOperatorStruct lambda = lambdaFunctionCall.getLambdaStruct();

			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel, lambda);

			final List<LispStruct> arguments = lambdaFunctionCall.getArguments();
			final List<LispStruct> evaluatedArguments = new ArrayList<>(arguments.size());
			for (final LispStruct argument : arguments) {
				final LispStruct evaluatedArgument = eval(argument);
				evaluatedArguments.add(evaluatedArgument);
			}

			final LispStruct[] args = new LispStruct[evaluatedArguments.size()];
			evaluatedArguments.toArray(args);

			return function.apply(args);
		}

		if (exp instanceof CompilerSpecialOperatorStruct) {
			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel, (CompilerSpecialOperatorStruct) exp);
			return function.apply();
		}

		return exp;
	}

	private FunctionStruct getCompiledExpression(final BooleanStruct oldCompileTopLevel, final CompilerSpecialOperatorStruct exp) {
		CompilerVariables.COMPILE_TOP_LEVEL.setValue(NILStruct.INSTANCE);

		final BooleanStruct oldConvertingForInterpreter = CompilerVariables.CONVERTING_FOR_INTERPRETER.getValue();
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
