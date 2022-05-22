/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.function.InternalCompile;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class CompileFilePathnameFunction extends BuiltInFunctionStructImpl {

	private static final String INPUT_FILE_ARGUMENT = "INPUT-FILE";

	public CompileFilePathnameFunction() {
		super("Returns the pathname that compile-file would write into, if given the same arguments.",
		      CommonLispSymbols.COMPILE_FILE_PATHNAME.getName(),
		      Parameters.forFunction(CommonLispSymbols.COMPILE_FILE_PATHNAME.getName())
		                .requiredParameter(INPUT_FILE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.OUTPUT_FILE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .allowOtherKeys()
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.COMPILE_FILE_PATHNAME;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct inputFile = arguments.getRequiredArgument(INPUT_FILE_ARGUMENT);
		if (arguments.hasKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD)) {
			final LispStruct outputFile = arguments.getKeyArgument(CommonLispSymbols.OUTPUT_FILE_KEYWORD);
			return InternalCompile.compileFilePathname(inputFile, outputFile);
		} else {
			return InternalCompile.compileFilePathname(inputFile, null);
		}
	}
}
