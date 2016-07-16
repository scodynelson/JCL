/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import jcl.lang.stream.OutputStream;
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
