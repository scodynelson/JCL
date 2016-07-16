/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.StreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class StreamElementTypeFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "STREAM-ELEMENT-TYPE";
	private static final String STREAM_ARGUMENT = "STREAM";

	public StreamElementTypeFunction() {
		super("Returns a type specifier that indicates the types of objects that may be read from or written to stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamStruct streamStruct = arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);
		return streamStruct.getElementType();
	}
}
