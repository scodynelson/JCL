/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class ZeroPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "ZEROP";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public ZeroPFunction() {
		super("Returns true if number is zero; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		final boolean result = number.zerop();
		return BooleanStructs.toLispBoolean(result);
	}
}
