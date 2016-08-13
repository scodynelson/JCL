/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class DivideFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "/";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public DivideFunction() {
		super("If no denominators are supplied, the function / returns the reciprocal of number. If more than one argument is given, the function / divides the numerator by all of the denominators and returns the resulting quotient.",
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

		return NumberStruct.divide(number, numbers);
	}
}
