/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberVariables;
import jcl.numbers.RandomStateStruct;
import jcl.numbers.RealStruct;
import jcl.numbers.SingleFloatStruct;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class RandomFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "RANDOM";
	private static final String LIMIT_ARGUMENT = "LIMIT";
	private static final String RANDOM_STATE_ARGUMENT = "RANDOM-STATE";

	@Autowired
	private Printer printer;

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
			final IntegerStruct number = (IntegerStruct) real;
			final BigInteger bigInteger = number.getBigInteger();
			final BigInteger randomInteger = randomState.randomInteger(bigInteger);
			return IntegerStruct.valueOf(randomInteger);
		} else if (real instanceof FloatStruct) {
			final FloatStruct number = (FloatStruct) real;
			final BigDecimal bigDecimal = number.getBigDecimal();
			final BigDecimal randomFloat = randomState.randomFloat(bigDecimal);
			return new SingleFloatStruct(randomFloat);
		} else {
			final String printedObject = printer.print(real);
			throw new TypeErrorException("Argument not of type Integer or Float: " + printedObject);
		}
	}
}
