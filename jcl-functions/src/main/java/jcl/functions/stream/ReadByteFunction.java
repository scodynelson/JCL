/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import java.math.BigInteger;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.ReadPeekResult;
import jcl.lang.stream.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class ReadByteFunction extends CommonLispBuiltInFunctionStruct {

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

		final InputStream inputStream;
		if (TStruct.INSTANCE.equals(lispStruct)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct)) {
			inputStream = StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStream) {
			inputStream = (InputStream) lispStruct;
		} else {
			throw new TypeErrorException("The value " + lispStruct + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);

		final ReadPeekResult readPeekResult = inputStream.readByte(eofErrorP.booleanValue(), eofValue);
		return readPeekResult.isEof() ? eofValue : IntegerStruct.valueOf(BigInteger.valueOf(readPeekResult.getResult()));
	}
}
