/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.FloatStruct;
import jcl.lang.number.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class FloatFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FLOAT";
	private static final String REAL_ARGUMENT = "REAL";
	private static final String PROTOTYPE_ARGUMENT = "PROTOTYPE";

	public FloatFunction() {
		super("Converts a real number to a float.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		                .optionalParameter(PROTOTYPE_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		if (arguments.hasOptionalArgument(PROTOTYPE_ARGUMENT)) {
			final FloatStruct prototype = arguments.getOptionalArgument(PROTOTYPE_ARGUMENT, FloatStruct.class);
			return real.floatingPoint(prototype);
		}
		return real.floatingPoint();
	}
}
