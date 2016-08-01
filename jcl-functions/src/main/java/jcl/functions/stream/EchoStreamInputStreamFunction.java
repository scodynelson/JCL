/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.EchoStreamStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class EchoStreamInputStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ECHO-STREAM-INPUT-STREAM";
	private static final String ECHO_STREAM_ARGUMENT = "ECHO-STREAM";

	public EchoStreamInputStreamFunction() {
		super("Returns the input stream from which echo-stream receives input.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(ECHO_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final EchoStreamStructImpl echoStream = arguments.getRequiredArgument(ECHO_STREAM_ARGUMENT, EchoStreamStructImpl.class);
		return echoStream.getInputStreamStruct();
	}
}
