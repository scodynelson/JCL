/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
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
		final IntegerStruct integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStruct.class);
		final IntegerStruct count = arguments.getRequiredArgument(COUNT_ARGUMENT, IntegerStruct.class);
		return integer.ash(count);
	}
}
