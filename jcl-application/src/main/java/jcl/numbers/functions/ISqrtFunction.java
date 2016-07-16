/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class ISqrtFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ISQRT";
	private static final String INTEGER_ARGUMENT = "INTEGER";

	public ISqrtFunction() {
		super("Returns the greatest integer less than or equal to the exact positive square root of natural.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INTEGER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStruct.class);
		return integer.isqrt();
	}
}
