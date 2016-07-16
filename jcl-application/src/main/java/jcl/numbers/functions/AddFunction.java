/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.NumberStruct;
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
