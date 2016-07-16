/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class ModFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MOD";
	private static final String REAL_ARGUMENT = "REAL";
	private static final String DIVISOR_ARGUMENT = "DIVISOR";

	public ModFunction() {
		super("Performs the operation floor on number and divisor and returns the remainder of the floor operation.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		                .requiredParameter(DIVISOR_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		final RealStruct divisor = arguments.getRequiredArgument(DIVISOR_ARGUMENT, RealStruct.class);
		return real.mod(divisor);
	}
}
