/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.BuiltInClassStruct;
import jcl.type.RandomStateType;
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
	 * TODO: fix??
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigInteger}
	 *
	 * @return the random {@link BigInteger}
	 */
	public BigInteger randomInteger(final long limit) {
		final long randomLong = RandomUtils.nextLong(0L, limit);
		return BigInteger.valueOf(randomLong);
	}

	/**
	 * Retrieves a random {@link BigDecimal} from the internal random seed.
	 * TODO: fix??
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigDecimal}
	 *
	 * @return the random {@link BigDecimal}
	 */
	public BigDecimal randomFloat(final double limit) {
		final double randomDouble = RandomUtils.nextDouble(0.0D, limit);
		return NumberUtils.bigDecimalValue(randomDouble);
	}
}
