/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class TwoWayStreamInputStreamFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "TWO-WAY-STREAM-INPUT-STREAM";
	private static final String TWO_WAY_STREAM_ARGUMENT = "TWO-WAY-STREAM";

	public TwoWayStreamInputStreamFunction() {
		super("Returns the input stream from which two-way-stream receives input.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(TWO_WAY_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final TwoWayStreamStruct twoWayStream = arguments.getRequiredArgument(TWO_WAY_STREAM_ARGUMENT, TwoWayStreamStruct.class);
		return twoWayStream.getInputStreamStruct();
	}
}
