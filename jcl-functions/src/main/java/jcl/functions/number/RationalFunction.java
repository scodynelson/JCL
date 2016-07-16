/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class RationalFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RATIONAL";
	private static final String REAL_ARGUMENT = "REAL";

	public RationalFunction() {
		super("Convert real to a rational.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		return real.rational();
	}
}
