/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.InternalRead;

public final class ReadFunction extends BuiltInFunctionStructImpl {

	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	public ReadFunction() {
		super("Parses the printed representation of an object from input-stream and builds such an object.",
		      CommonLispSymbols.READ.getName(),
		      Parameters.forFunction(CommonLispSymbols.READ.getName())
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(CommonLispSymbols.STANDARD_INPUT)
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(RECURSIVE_P_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.READ;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct inputStreamArg = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT);
		final InputStreamStruct inputStreamStruct;
		if (TStruct.INSTANCE.eq(inputStreamArg)) {
			inputStreamStruct = (InputStreamStruct) CommonLispSymbols.TERMINAL_IO.symbolValue();
		} else if (NILStruct.INSTANCE.eq(inputStreamArg)) {
			inputStreamStruct = (InputStreamStruct) CommonLispSymbols.STANDARD_INPUT.symbolValue();
		} else if (inputStreamArg instanceof InputStreamStruct) {
			inputStreamStruct = (InputStreamStruct) inputStreamArg;
		} else {
			throw new TypeErrorException("The value " + inputStreamArg + " is not either T, NIL, or a STREAM.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		return InternalRead.read(inputStreamStruct, eofErrorP, eofValue, recursiveP);
	}
}
