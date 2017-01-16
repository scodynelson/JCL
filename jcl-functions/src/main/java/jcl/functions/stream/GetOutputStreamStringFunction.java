/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StringOutputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class GetOutputStreamStringFunction extends CommonLispBuiltInFunctionStructBase {

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

		return StringStruct.toLispString(streamString);
	}
}
