/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.NumberVariables;
import jcl.lang.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class RandomFunction extends CommonLispBuiltInFunctionStructBase {

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
		final RandomStateStruct randomState = arguments.getRequiredArgument(RANDOM_STATE_ARGUMENT, RandomStateStruct.class);

		if (real instanceof IntegerStruct) {
			// TODO: fix??
			final IntegerStruct number = (IntegerStruct) real;
			final BigInteger randomInteger = randomState.randomInteger(number.longValue());
			return LispStructFactory.toInteger(randomInteger);
		} else if (real instanceof FloatStruct) {
			// TODO: fix??
			final FloatStruct number = (FloatStruct) real;
			final BigDecimal randomFloat = randomState.randomFloat(number.doubleValue());
			return LispStructFactory.toFloat(randomFloat.doubleValue());
		} else {
			throw new TypeErrorException("Argument not of type Integer or Float: " + real);
		}
	}
}
