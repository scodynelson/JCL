/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.math.BigInteger;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.stream.ReadPeekResult;
import jcl.lang.statics.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class ReadByteFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "READ-BYTE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";

	public ReadByteFunction() {
		super("Returns the next byte from input-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INPUT_STREAM_ARGUMENT)
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct lispStruct = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT);

		final InputStreamStruct inputStreamStruct;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStreamStruct) {
			inputStreamStruct = (InputStreamStruct) lispStruct;
		} else {
			throw new TypeErrorException("The value " + lispStruct + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);

		final ReadPeekResult readPeekResult = inputStreamStruct.readByte(eofErrorP.booleanValue(), eofValue);
		return readPeekResult.isEof() ? eofValue : LispStructFactory.toInteger(BigInteger.valueOf(readPeekResult.getResult()));
	}
}
