/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class NotEqualToFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "/=";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public NotEqualToFunction() {
		super("Returns true if no two numbers are the same in value; otherwise returns false.",
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

		final boolean result = NumberStruct.isNotEqualTo(number, numbers);
		return LispStructFactory.toBoolean(result);
	}
}
