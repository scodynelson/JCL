/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.FloatStruct;
import org.springframework.stereotype.Component;

@Component
public final class FloatPrecisionFunction extends CommonLispBuiltInFunctionStruct {

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
