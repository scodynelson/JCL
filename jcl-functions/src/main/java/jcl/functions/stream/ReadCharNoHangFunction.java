/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.character.CharacterStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.ReadPeekResult;
import jcl.lang.stream.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class ReadCharNoHangFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "READ-CHAR-NO-HANG";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";
	private static final String RECURSIVE_P_ARGUMENT = "RECURSIVE-P";

	public ReadCharNoHangFunction() {
		super("Returns the next character from input-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT.getVariableValue())
		                .optionalParameter(EOF_ERROR_ARGUMENT).withInitialValue(TStruct.INSTANCE)
		                .optionalParameter(EOF_VALUE_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(RECURSIVE_P_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
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
		final BooleanStruct recursiveP = arguments.getOptionalArgument(RECURSIVE_P_ARGUMENT, BooleanStruct.class);

		final ReadPeekResult readPeekResult = inputStream.readChar(eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
		return readPeekResult.isEof() ? eofValue : CharacterStruct.valueOf(readPeekResult.getResult());
	}
}
