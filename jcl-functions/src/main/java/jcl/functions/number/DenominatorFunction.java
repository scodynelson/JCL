/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.RationalStruct;
import org.springframework.stereotype.Component;

@Component
public final class DenominatorFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "DENOMINATOR";
	private static final String RATIONAL_ARGUMENT = "RATIONAL";

	public DenominatorFunction() {
		super("Returns the denominator of the canonical form of rational.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(RATIONAL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RationalStruct rational = arguments.getRequiredArgument(RATIONAL_ARGUMENT, RationalStruct.class);
		return rational.denominator();
	}
}
