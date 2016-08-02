/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.FloatStructImpl;
import jcl.lang.number.IntegerStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class FloatSignFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FLOAT-SIGN";
	private static final String FLOAT1_ARGUMENT = "FLOAT1";
	private static final String FLOAT2_ARGUMENT = "FLOAT2";

	public FloatSignFunction() {
		super("Returns a number z such that z and float-1 have the same sign and also such that z and float-2 have the same absolute value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FLOAT1_ARGUMENT)
		                .optionalParameter(FLOAT2_ARGUMENT).withInitialValue(IntegerStructImpl.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		if (arguments.hasOptionalArgument(FLOAT2_ARGUMENT)) {
			final FloatStructImpl float1 = arguments.getRequiredArgument(FLOAT1_ARGUMENT, FloatStructImpl.class);
			final FloatStructImpl float2 = arguments.getOptionalArgument(FLOAT2_ARGUMENT, FloatStructImpl.class);
			return float1.floatSign(float2);
		} else {
			final FloatStructImpl float1 = arguments.getRequiredArgument(FLOAT1_ARGUMENT, FloatStructImpl.class);
			return float1.floatSign();
		}
	}
}
