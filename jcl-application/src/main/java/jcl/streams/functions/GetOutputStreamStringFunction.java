/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.StringOutputStreamStruct;
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
		final StringOutputStreamStruct stringOutputStream = arguments.getRequiredArgument(STRING_OUTPUT_STREAM_ARGUMENT, StringOutputStreamStruct.class);
		final String streamString = stringOutputStream.getStreamString();
		stringOutputStream.clearStream();

		return new StringStruct(streamString);
	}
}
