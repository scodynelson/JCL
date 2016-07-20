/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class MinusPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MINUSP";
	private static final String REAL_ARGUMENT = "REAL";

	public MinusPFunction() {
		super("Returns true if real is less than zero; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT));
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		final boolean result = real.minusp();
		return BooleanStructs.toLispBoolean(result);
	}
}