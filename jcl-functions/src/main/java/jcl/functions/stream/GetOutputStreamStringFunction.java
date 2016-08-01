/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.StringOutputStreamStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class GetOutputStreamStringFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "GET-OUTPUT-STREAM-STRING";
	private static final String STRING_OUTPUT_STREAM_ARGUMENT = "STRING-OUTPUT-STREAM";

	public GetOutputStreamStringFunction() {
		super("Returns a string containing, in order, all the characters that have been output to string-output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRING_OUTPUT_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringOutputStreamStructImpl stringOutputStream = arguments.getRequiredArgument(STRING_OUTPUT_STREAM_ARGUMENT, StringOutputStreamStructImpl.class);
		final String streamString = stringOutputStream.getStreamString();
		stringOutputStream.clearStream();

		return LispStructFactory.toString(streamString);
	}
}
