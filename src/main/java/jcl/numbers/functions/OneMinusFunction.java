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
public final class OneMinusFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "1-";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public OneMinusFunction() {
		super("Returns a number that is one less than its argument number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		return number.subtract(IntegerStruct.ONE);
	}
}