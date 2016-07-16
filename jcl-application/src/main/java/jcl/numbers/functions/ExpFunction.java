/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class ExpFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "EXP";
	private static final String POWER_NUMBER_ARGUMENT = "POWER-NUMBER";

	public ExpFunction() {
		super("Returns e raised to the power power-number, where e is the base of the natural logarithms.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(POWER_NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct powerNumber = arguments.getRequiredArgument(POWER_NUMBER_ARGUMENT, NumberStruct.class);
		return powerNumber.exp();
	}
}
