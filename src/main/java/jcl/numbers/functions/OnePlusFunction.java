/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class OnePlusFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "1+";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public OnePlusFunction() {
		super("Returns a number that is one more than its argument number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		return number.add(IntegerStruct.ONE);
	}
}
