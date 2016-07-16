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
public final class AbsFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ABS";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public AbsFunction() {
		super("Returns the absolute value of number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		return number.abs();
	}
}
