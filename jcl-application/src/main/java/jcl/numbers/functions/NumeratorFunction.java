/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.RationalStruct;
import org.springframework.stereotype.Component;

@Component
public final class NumeratorFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "NUMERATOR";
	private static final String RATIONAL_ARGUMENT = "RATIONAL";

	public NumeratorFunction() {
		super("Returns the numerator of the canonical form of rational.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(RATIONAL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RationalStruct rational = arguments.getRequiredArgument(RATIONAL_ARGUMENT, RationalStruct.class);
		return rational.numerator();
	}
}
