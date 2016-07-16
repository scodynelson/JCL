/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import jcl.lang.number.QuotientRemainder;
import jcl.lang.number.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class FloorFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FLOOR";
	private static final String REAL_ARGUMENT = "REAL";
	private static final String DIVISOR_ARGUMENT = "DIVISOR";

	public FloorFunction() {
		super("Produces a quotient that has been truncated toward negative infinity; that is, the quotient represents the largest mathematical integer that is not larger than the mathematical quotient.",
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

		final QuotientRemainder floor = real.floor(divisor);
		final RealStruct quotient = floor.getQuotient();
		final RealStruct remainder = floor.getRemainder();
		return new ValuesStruct(quotient, remainder);
	}
}
