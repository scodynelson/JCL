/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class SubtractFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "-";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public SubtractFunction() {
		super("If only one number is supplied, the negation of that number is returned. If more than one argument is given, it subtracts all of the subtrahends from the minuend and returns the result.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		final List<NumberStruct> numbers = arguments.getRestArgument(NumberStruct.class);

		return NumberStruct.subtract(number, numbers);
	}
}
