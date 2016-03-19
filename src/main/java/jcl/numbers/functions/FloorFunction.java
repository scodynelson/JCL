/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.numbers.QuotientRemainderResult;
import jcl.numbers.RealStruct;
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

		final QuotientRemainderResult floor = real.floor(divisor);
		final RealStruct quotient = floor.getQuotient();
		final RealStruct remainder = floor.getRemainder();
		return new ValuesStruct(quotient, remainder);
	}
}
