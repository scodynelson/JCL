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
public final class ACosHFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ACOSH";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public ACosHFunction() {
		super("Returns the hyperbolic-arc-cosine of number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		return number.acosh();
	}
}
