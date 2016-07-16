/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class RationalizeFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RATIONALIZE";
	private static final String REAL_ARGUMENT = "REAL";

	public RationalizeFunction() {
		super("Convert real to a rational.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		// TODO: Do we do anything different here???
		return real.rational();
	}
}
