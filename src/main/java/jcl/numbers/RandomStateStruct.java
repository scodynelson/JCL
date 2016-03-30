/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.classes.BuiltInClassStruct;
import jcl.types.RandomStateType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.RandomUtils;

/**
 * The {@link RandomStateStruct} is the object representation of a Lisp 'random-state' type.
 */
public class RandomStateStruct extends BuiltInClassStruct {

	/**
	 * Public constructor.
	 */
	public RandomStateStruct() {
		super(RandomStateType.INSTANCE, null, null);
	}

	/**
	 * Retrieves a random {@link BigInteger} from the internal random seed.
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigInteger}
	 *
	 * @return the random {@link BigInteger}
	 */
	public BigInteger randomInteger(final BigInteger limit) {

		final long limitLong = limit.longValue();
		final long randomLong = RandomUtils.nextLong(0L, limitLong);
		return BigInteger.valueOf(randomLong);
	}

	/**
	 * Retrieves a random {@link BigDecimal} from the internal random seed.
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigDecimal}
	 *
	 * @return the random {@link BigDecimal}
	 */
	public BigDecimal randomFloat(final BigDecimal limit) {

		final double limitDouble = limit.doubleValue();
		final double randomDouble = RandomUtils.nextDouble(0.0D, limitDouble);
		return NumberUtils.bigDecimalValue(randomDouble);
	}
}
