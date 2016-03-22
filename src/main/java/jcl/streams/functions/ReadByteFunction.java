/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntIntegerStruct;
import jcl.printer.Printer;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ReadByteFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "READ-BYTE";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String EOF_ERROR_ARGUMENT = "EOF-ERROR";
	private static final String EOF_VALUE_ARGUMENT = "EOF-VALUE";

	@Autowired
	private Printer printer;

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
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("The value " + printedObject + " is not either T, NIL, or an Input Stream.");
		}

		final BooleanStruct eofErrorP = arguments.getOptionalArgument(EOF_ERROR_ARGUMENT, BooleanStruct.class);
		final LispStruct eofValue = arguments.getOptionalArgument(EOF_VALUE_ARGUMENT);

		final ReadPeekResult readPeekResult = inputStream.readByte(eofErrorP.booleanValue(), eofValue);
		return readPeekResult.isEof() ? eofValue : new IntIntegerStruct(BigInteger.valueOf(readPeekResult.getResult()));
	}
}
