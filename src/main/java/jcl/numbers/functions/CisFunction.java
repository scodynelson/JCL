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
public final class CisFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "CIS";
	private static final String REAL_ARGUMENT = "REAL";

	public CisFunction() {
		super("Returns the value of e^i* radians, which is a complex in which the real part is equal to the cosine of radians, and the imaginary part is equal to the sine of radians.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		return real.cis();
	}
}
