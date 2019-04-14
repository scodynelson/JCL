package jcl.functions;

import jcl.compiler.function.InternalCompile;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.CompilerVariables;

public final class CompileFileFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "COMPILE-FILE";
	private static final String INPUT_FILE_ARGUMENT = "INPUT-FILE";

	public CompileFileFunction() {
		super("Compiles the provided input-file.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INPUT_FILE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.OUTPUT_FILE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.VERBOSE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.PRINT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct inputFile = arguments.getRequiredArgument(INPUT_FILE_ARGUMENT);
		final LispStruct outputFile = arguments.getKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD);
		final BooleanStruct verbose;
		if (arguments.hasKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD)) {
			verbose = arguments.getKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD, BooleanStruct.class);
		} else {
			verbose = CompilerVariables.COMPILE_VERBOSE.getVariableValue();
		}
		final BooleanStruct print;
		if (arguments.hasKeyArgument(CommonLispSymbols.PRINT_KEYWORD)) {
			print = arguments.getKeyArgument(CommonLispSymbols.PRINT_KEYWORD, BooleanStruct.class);
		} else {
			print = CompilerVariables.COMPILE_PRINT.getVariableValue();
		}
		final LispStruct externalFormat = arguments.getKeyArgument(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);
		return InternalCompile.compileFile(inputFile, outputFile, verbose, print, externalFormat);
	}
}
