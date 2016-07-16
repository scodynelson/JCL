/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.FloatStruct;
import jcl.lang.number.IntegerStruct;
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
		                .optionalParameter(FLOAT2_ARGUMENT).withInitialValue(IntegerStruct.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		if (arguments.hasOptionalArgument(FLOAT2_ARGUMENT)) {
			final FloatStruct float1 = arguments.getRequiredArgument(FLOAT1_ARGUMENT, FloatStruct.class);
			final FloatStruct float2 = arguments.getOptionalArgument(FLOAT2_ARGUMENT, FloatStruct.class);
			return float1.floatSign(float2);
		} else {
			final FloatStruct float1 = arguments.getRequiredArgument(FLOAT1_ARGUMENT, FloatStruct.class);
			return float1.floatSign();
		}
	}
}
