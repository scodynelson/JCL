/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ScaleFloatFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SCALE-FLOAT";
	private static final String FLOAT_ARGUMENT = "FLOAT";
	private static final String SCALE_ARGUMENT = "SCALE";

	public ScaleFloatFunction() {
		super("Returns (* float (expt (float b float) integer)), where b is the radix of the floating-point representation. float is not necessarily between 1/b and 1.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FLOAT_ARGUMENT)
		                .requiredParameter(SCALE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FloatStruct floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStruct.class);
		final IntegerStruct scale = arguments.getRequiredArgument(SCALE_ARGUMENT, IntegerStruct.class);
		return floatVal.scaleFloat(scale);
	}
}
