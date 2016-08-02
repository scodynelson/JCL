/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class LogCountFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOGCOUNT";
	private static final String INTEGER_ARGUMENT = "INTEGER";

	public LogCountFunction() {
		super("Computes and returns the number of bits in the two's-complement binary representation of integer that are `on' or `set'. If integer is negative, the 0 bits are counted; otherwise, the 1 bits are counted.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INTEGER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStructImpl integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStructImpl.class);
		return integer.logCount();
	}
}
