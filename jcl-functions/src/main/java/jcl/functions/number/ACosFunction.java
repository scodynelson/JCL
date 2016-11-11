/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class ACosFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "ACOS";
	private static final String NUMBER_ARGUMENT = "NUMBER";

	public ACosFunction() {
		super("Returns the arc-cosine of number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
		return number.acos();
	}
}
