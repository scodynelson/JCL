/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.StreamVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ReadFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "READ";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	@Autowired
	private Reader reader;

	public ReadFunction() {
		super("Parses the printed representation of an object from input-stream and builds such an object.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT)
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(RECURSIVE_P_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct inputStreamArg = arguments.getRequiredArgument(INPUT_STREAM_ARGUMENT);
		final InputStreamStruct inputStreamStruct;
		if (TStruct.INSTANCE.equals(inputStreamArg)) {
			inputStreamStruct = StreamVariables.TERMINAL_IO.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(inputStreamArg)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (inputStreamArg instanceof InputStreamStruct) {
			inputStreamStruct = (InputStreamStruct) inputStreamArg;
		} else {
			throw new TypeErrorException("The value " + inputStreamArg + " is not either T, NIL, or a STREAM.");
		}

		final ReaderInputStreamStruct readerInputStreamStruct = new ReaderInputStreamStruct(inputStreamStruct);
		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		return read(readerInputStreamStruct, eofErrorP, eofValue, recursiveP);
	}

	public LispStruct read(final ReaderInputStreamStruct inputStreamStruct, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                       final BooleanStruct recursiveP) {

		return read(inputStreamStruct, eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}

	public LispStruct read(final ReaderInputStreamStruct inputStreamStruct, final boolean eofErrorP, final LispStruct eofValue,
	                       final boolean recursiveP) {

		return reader.read(inputStreamStruct, eofErrorP, eofValue, recursiveP);
	}
}
