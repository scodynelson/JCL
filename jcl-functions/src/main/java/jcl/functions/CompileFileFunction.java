package jcl.functions;

import jcl.compiler.function.CompileFileResult;
import jcl.compiler.function.InternalCompile;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class CompileFileFunction extends BuiltInFunctionStructImpl {

	private static final String INPUT_FILE_ARGUMENT = "INPUT-FILE";

	public CompileFileFunction() {
		super("Compiles the provided input-file.",
		      CommonLispSymbols.COMPILE_FILE.getName(),
		      Parameters.forFunction(CommonLispSymbols.COMPILE_FILE.getName())
		                .requiredParameter(INPUT_FILE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.OUTPUT_FILE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.VERBOSE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.PRINT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.COMPILE_FILE;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct inputFile = arguments.getRequiredArgument(INPUT_FILE_ARGUMENT);
		final LispStruct outputFile = arguments.getKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD);
		final BooleanStruct verbose;
		if (arguments.hasKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD)) {
			verbose = arguments.getKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD, BooleanStruct.class);
		} else {
			verbose = CommonLispSymbols.COMPILE_VERBOSE_VAR.getVariableValue();
		}
		final BooleanStruct print;
		if (arguments.hasKeyArgument(CommonLispSymbols.PRINT_KEYWORD)) {
			print = arguments.getKeyArgument(CommonLispSymbols.PRINT_KEYWORD, BooleanStruct.class);
		} else {
			print = CommonLispSymbols.COMPILE_PRINT_VAR.getVariableValue();
		}
		final LispStruct externalFormat = arguments.getKeyArgument(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);
		final CompileFileResult compileFileResult = InternalCompile.compileFile(inputFile, outputFile, verbose, print, externalFormat);
		return compileFileResult.toValues();
	}
}
