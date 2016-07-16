/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class EvenPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "EVENP";
	private static final String INTEGER_ARGUMENT = "INTEGER";

	public EvenPFunction() {
		super("Returns true if integer is even (divisible by two); otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INTEGER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStruct.class);
		final boolean result = integer.evenp();
		return BooleanStructs.toLispBoolean(result);
	}
}
