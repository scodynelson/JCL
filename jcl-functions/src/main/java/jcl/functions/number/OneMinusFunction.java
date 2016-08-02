/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStructImpl;
import jcl.lang.NumberStruct;
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
		return number.subtract(IntegerStructImpl.ONE);
	}
}
