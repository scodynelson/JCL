/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.QuotientRemainder;
import jcl.lang.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class RoundFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "ROUND";
	private static final String REAL_ARGUMENT = "REAL";
	private static final String DIVISOR_ARGUMENT = "DIVISOR";

	public RoundFunction() {
		super("Produces a quotient that has been rounded to the nearest mathematical integer; if the mathematical quotient is exactly halfway between two integers, (that is, it has the form integer+1/2), then the quotient has been rounded to the even (divisible by two) integer.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		                .optionalParameter(DIVISOR_ARGUMENT).withInitialValue(IntegerStruct.ONE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		final RealStruct divisor = arguments.getRequiredArgument(DIVISOR_ARGUMENT, RealStruct.class);

		final QuotientRemainder round = real.round(divisor);
		final RealStruct quotient = round.getQuotient();
		final RealStruct remainder = round.getRemainder();
		return ValuesStruct.valueOf(quotient, remainder);
	}
}
