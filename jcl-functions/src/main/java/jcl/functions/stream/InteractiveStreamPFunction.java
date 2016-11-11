/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StreamStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class InteractiveStreamPFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "INTERACTIVE-STREAM-P";
	private static final String STREAM_ARGUMENT = "STREAM";

	public InteractiveStreamPFunction() {
		super("Returns true if stream is an interactive stream; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamStruct stream = arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);
		return LispStructFactory.toBoolean(stream.isInteractive());
	}
}
