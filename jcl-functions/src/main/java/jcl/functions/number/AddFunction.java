/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class AddFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "+";

	public AddFunction() {
		super("Returns the sum of numbers, performing any necessary type conversions in the process. If no numbers are supplied, 0 is returned.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<NumberStruct> numbers = arguments.getRestArgument(NumberStruct.class);
		return NumberStruct.add(numbers);
	}
}