/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StreamStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code stream-error-stream}.
 */
@Component
public final class StreamErrorStreamFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "STREAM-ERROR-STREAM";
	private static final String CONDITION_ARGUMENT = "CONDITION";

	/**
	 * Public constructor passing the documentation string.
	 */
	public StreamErrorStreamFunction() {
		super("Returns the offending stream in the situation represented by the condition.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CONDITION_ARGUMENT)
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for {@code stream-error-stream} package function that returns the {@link StreamStruct} that
	 * was a part of provided {@link StreamErrorException} condition.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the {@link StreamStruct} that was a part of provided {@link StreamErrorException} condition
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamErrorException streamErrorException = arguments.getRequiredArgument(CONDITION_ARGUMENT, StreamErrorException.class);
		return streamErrorException.getStreamWithError();
	}
}
