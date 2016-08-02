/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.FloatStructImpl;
import jcl.lang.number.IntegerStructImpl;
import jcl.lang.statics.NumberVariables;
import jcl.lang.number.RandomStateStructImpl;
import jcl.lang.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class RandomFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RANDOM";
	private static final String LIMIT_ARGUMENT = "LIMIT";
	private static final String RANDOM_STATE_ARGUMENT = "RANDOM-STATE";

	public RandomFunction() {
		super("Returns a pseudo-random number that is a non-negative number less than limit and of the same type as limit.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIMIT_ARGUMENT)
		                .optionalParameter(RANDOM_STATE_ARGUMENT).withInitialValue(NumberVariables.RANDOM_STATE.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(LIMIT_ARGUMENT, RealStruct.class);
		final RandomStateStructImpl randomState = arguments.getRequiredArgument(RANDOM_STATE_ARGUMENT, RandomStateStructImpl.class);

		if (real instanceof IntegerStructImpl) {
			// TODO: fix??
			final IntegerStructImpl number = (IntegerStructImpl) real;
			final BigInteger randomInteger = randomState.randomInteger(number.longValue());
			return IntegerStructImpl.valueOf(randomInteger);
		} else if (real instanceof FloatStructImpl) {
			// TODO: fix??
			final FloatStructImpl number = (FloatStructImpl) real;
			final BigDecimal randomFloat = randomState.randomFloat(number.doubleValue());
			return FloatStructImpl.valueOf(randomFloat.doubleValue());
		} else {
			throw new TypeErrorException("Argument not of type Integer or Float: " + real);
		}
	}
}
