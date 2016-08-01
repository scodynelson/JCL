/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.OutputStream;
import org.springframework.stereotype.Component;

@Component
public final class MakeEchoStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-ECHO-STREAM";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public MakeEchoStreamFunction() {
		super("Returns a echo stream that gets its input from input-stream and sends its output to output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INPUT_STREAM_ARGUMENT)
		                .requiredParameter(OUTPUT_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final InputStream inputStream = arguments.getRequiredArgument(INPUT_STREAM_ARGUMENT, InputStream.class);
		final OutputStream outputStream = arguments.getRequiredArgument(OUTPUT_STREAM_ARGUMENT, OutputStream.class);
		return LispStructFactory.toEchoStream(inputStream, outputStream);
	}
}
