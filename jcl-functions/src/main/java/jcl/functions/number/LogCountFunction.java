/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class LogCountFunction extends CommonLispBuiltInFunctionStructBase {

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
		final IntegerStruct integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStruct.class);
		return integer.logCount();
	}
}
