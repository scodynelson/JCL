/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.StreamStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class StreamElementTypeFunction extends CommonLispBuiltInFunctionStructBase {

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
