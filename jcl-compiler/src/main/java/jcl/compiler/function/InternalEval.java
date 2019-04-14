package jcl.compiler.function;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import jcl.compiler.environment.Environment;
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
import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.java.JavaNameStruct;
import jcl.lang.java.JavaObjectStruct;
import jcl.lang.statics.CompilerVariables;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@UtilityClass
public final class InternalEval {

	public static LispStruct eval(final LispStruct originalExp) {

		final Environment nullEnvironment = Environment.NULL;
		return eval(originalExp, nullEnvironment);
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	public static LispStruct eval(final LispStruct originalExp, final Environment environment) {

		final BooleanStruct oldCompileTopLevel = CompilerVariables.COMPILE_TOP_LEVEL.getVariableValue();
		CompilerVariables.COMPILE_TOP_LEVEL.setValue(NILStruct.INSTANCE);

		LispStruct exp;
		try {
			exp = FormAnalyzer.analyze(originalExp, environment);
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

			LispStruct finalForm = NILStruct.INSTANCE;

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

			LispStruct finalForm = NILStruct.INSTANCE;

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

//			final long start = System.nanoTime();
			final LispStruct apply = function.apply(args);
//			final long end = System.nanoTime();
//			System.out.println("EVAL: " + (end - start));
			return apply;
		}

		if (exp instanceof JavaMethodCallStruct) {
			final JavaMethodCallStruct javaMethodCall = (JavaMethodCallStruct) exp;
			final JavaNameStruct methodName = javaMethodCall.getMethodName();

			final LispStruct javaObject = javaMethodCall.getJavaObject();
			final LispStruct evaluatedJavaObject = eval(javaObject);

			final Object actualJavaObject ;
			if (evaluatedJavaObject instanceof JavaObjectStruct) {
				final JavaObjectStruct javaObjectStruct = (JavaObjectStruct) evaluatedJavaObject;
				actualJavaObject = javaObjectStruct.getJavaObject();
			} else {
				actualJavaObject = evaluatedJavaObject;
			}

			final List<LispStruct> arguments = javaMethodCall.getArguments();

			final Object[] methodEvaluatedArgs = new Object[arguments.size()];
			final Class<?>[] methodParamTypes = new Class[arguments.size()];
			for (int i = 0; i < arguments.size(); i++) {
				final LispStruct currentArg = arguments.get(i);
				final LispStruct evaluatedArgument = eval(currentArg);
				methodEvaluatedArgs[i] = evaluatedArgument;
				methodParamTypes[i] = evaluatedArgument.getClass();
			}

			final JavaMethodStruct javaMethodStruct = JavaMethodStruct.toJavaMethod(methodName.getJavaName(),
			                                                                         actualJavaObject.getClass(),
			                                                                         methodParamTypes);
			final Method javaMethod = javaMethodStruct.getJavaMethod();

			return jInvoke(javaMethod, actualJavaObject, methodEvaluatedArgs);
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
			final FunctionStruct function = getCompiledExpression(oldCompileTopLevel,
			                                                      (CompilerSpecialOperatorStruct) exp);
			return function.apply();
		}

		return exp;
	}

	private static FunctionStruct getCompiledExpression(final BooleanStruct oldCompileTopLevel,
	                                                    final CompilerSpecialOperatorStruct exp) {
		CompilerVariables.COMPILE_TOP_LEVEL.setValue(NILStruct.INSTANCE);

		final BooleanStruct oldConvertingForInterpreter = CompilerVariables.CONVERTING_FOR_INTERPRETER.getVariableValue();
		CompilerVariables.CONVERTING_FOR_INTERPRETER.setValue(TStruct.INSTANCE);

		final FunctionStruct function;
		try {
			final CompileResult compileResult = CompileForm.compile(exp);
			function = compileResult.getFunction();
		} finally {
			CompilerVariables.CONVERTING_FOR_INTERPRETER.setValue(oldConvertingForInterpreter);
			CompilerVariables.COMPILE_TOP_LEVEL.setValue(oldCompileTopLevel);
		}
		return function;
	}

	public static LispStruct jInvoke(final Method javaMethod, final Object javaObject, final Object... methodArgs) {

		final String javaMethodName = javaMethod.getName();

		try {
			final Object methodResult = javaMethod.invoke(javaObject, methodArgs);
			if (methodResult instanceof LispStruct) {
				return (LispStruct) methodResult;
			}
			return JavaObjectStruct.toJavaObject(methodResult);
		} catch (final InvocationTargetException | IllegalAccessException ex) {
			String message = "Java Method '" + javaMethodName + "' could not be properly invoked";

			if (javaObject != null) {
				final String javaObjectClassName = javaObject.getClass().getName();
				message += " on Java Class '" + javaObjectClassName + '\'';
			}
			message += '.';

			log.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}
}
