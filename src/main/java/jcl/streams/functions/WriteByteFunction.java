/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.streams.OutputStream;
import org.springframework.stereotype.Component;

@Component
public final class WriteByteFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "WRITE-BYTE";
	private static final String BYTE_ARGUMENT = "BYTE";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public WriteByteFunction() {
		super("Outputs the byte to the output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(BYTE_ARGUMENT)
		                .requiredParameter(OUTPUT_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct byteVal = arguments.getRequiredArgument(BYTE_ARGUMENT, IntegerStruct.class);
		final OutputStream outputStream = arguments.getRequiredArgument(OUTPUT_STREAM_ARGUMENT, OutputStream.class);

		final int byteValue = byteVal.intValue();
		outputStream.writeByte(byteValue);

		return byteVal;
	}
}
