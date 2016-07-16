/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class GcdFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "GCD";

	public GcdFunction() {
		super("Returns the greatest common divisor of integers. If only one integer is supplied, its absolute value is returned. If no integers are given, gcd returns 0.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<IntegerStruct> integers = arguments.getRestArgument(IntegerStruct.class);
		return IntegerStruct.gcd(integers);
	}
}
