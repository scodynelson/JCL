/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.StreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class StreamPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "STREAMP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public StreamPFunction() {
		super("Returns true if object is of type stream; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return BooleanStructs.toLispBoolean(object instanceof StreamStruct);
	}
}
