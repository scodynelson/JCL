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
public final class AshFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ASH";
	private static final String INTEGER_ARGUMENT = "INTEGER";
	private static final String COUNT_ARGUMENT = "COUNT";

	public AshFunction() {
		super("Shifts integer arithmetically left by count bit positions if count is positive, or right count bit positions if count is negative.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INTEGER_ARGUMENT)
		                .requiredParameter(COUNT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStructImpl integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStructImpl.class);
		final IntegerStructImpl count = arguments.getRequiredArgument(COUNT_ARGUMENT, IntegerStructImpl.class);
		return integer.ash(count);
	}
}
