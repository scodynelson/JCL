/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class ExptFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "EXPT";
	private static final String BASE_NUMBER_ARGUMENT = "BASE-NUMBER";
	private static final String POWER_NUMBER_ARGUMENT = "POWER-NUMBER";

	public ExptFunction() {
		super("Returns base-number raised to the power power-number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(BASE_NUMBER_ARGUMENT)
		                .requiredParameter(POWER_NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct baseNumber = arguments.getRequiredArgument(BASE_NUMBER_ARGUMENT, NumberStruct.class);
		final NumberStruct powerNumber = arguments.getRequiredArgument(POWER_NUMBER_ARGUMENT, NumberStruct.class);
		return baseNumber.expt(powerNumber);
	}
}
