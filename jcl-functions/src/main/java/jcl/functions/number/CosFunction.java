/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class CosFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "COS";
	private static final String RADIANS_ARGUMENT = "RADIANS";

	public CosFunction() {
		super("Returns the cosine of radians.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(RADIANS_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct radians = arguments.getRequiredArgument(RADIANS_ARGUMENT, NumberStruct.class);
		return radians.cos();
	}
}
