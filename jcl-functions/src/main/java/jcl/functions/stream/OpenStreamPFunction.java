/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.StreamStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class OpenStreamPFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "OPEN-STREAM-P";
	private static final String STREAM_ARGUMENT = "STREAM";

	public OpenStreamPFunction() {
		super("Returns true if stream is an open stream; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamStruct stream = arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);
		return BooleanStruct.toLispBoolean(!stream.isClosed());
	}
}
