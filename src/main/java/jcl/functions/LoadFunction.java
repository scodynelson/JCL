/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.compiler.function.InternalLoad;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class LoadFunction extends BuiltInFunctionStructImpl {

	private static final String FILESPEC_ARGUMENT = "FILESPEC";

	public LoadFunction() {
		super("Loads the file named by filespec into the Lisp environment.",
		      CommonLispSymbols.LOAD.getName(),
		      Parameters.forFunction(CommonLispSymbols.LOAD.getName())
		                .requiredParameter(FILESPEC_ARGUMENT)
		                .keyParameter(CommonLispSymbols.VERBOSE_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.PRINT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD).withInitialValue(TStruct.INSTANCE)
		                .keyParameter(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.LOAD;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct filespec = arguments.getRequiredArgument(FILESPEC_ARGUMENT);
		final BooleanStruct verbose;
		if (arguments.hasKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD)) {
			verbose = arguments.getKeyArgument(CommonLispSymbols.VERBOSE_KEYWORD, BooleanStruct.class);
		} else {
			verbose = CommonLispSymbols.LOAD_VERBOSE_VAR.getVariableValue();
		}
		final BooleanStruct print;
		if (arguments.hasKeyArgument(CommonLispSymbols.PRINT_KEYWORD)) {
			print = arguments.getKeyArgument(CommonLispSymbols.PRINT_KEYWORD, BooleanStruct.class);
		} else {
			print = CommonLispSymbols.LOAD_PRINT_VAR.getVariableValue();
		}
		final BooleanStruct ifDoesNotExist = arguments.getKeyArgument(CommonLispSymbols.IF_DOES_NOT_EXIST_KEYWORD, BooleanStruct.class);
		final LispStruct externalFormat = arguments.getKeyArgument(CommonLispSymbols.EXTERNAL_FORMAT_KEYWORD);
		return InternalLoad.load(filespec, verbose, print, ifDoesNotExist, externalFormat);
	}
}
