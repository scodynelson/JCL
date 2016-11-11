/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.FloatStruct;
import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class FloatPrecisionFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "FLOAT-PRECISION";
	private static final String FLOAT_ARGUMENT = "FLOAT";

	public FloatPrecisionFunction() {
		super("Returns the number of significant radix b digits present in float; if float is a float zero, then the result is an integer zero.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FLOAT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FloatStruct floatVal = arguments.getRequiredArgument(FLOAT_ARGUMENT, FloatStruct.class);
		return floatVal.floatPrecision();
	}
}
