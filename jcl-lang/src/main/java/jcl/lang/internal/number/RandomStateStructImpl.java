/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.BuiltInClassStruct;
import jcl.lang.RandomStateStruct;
import jcl.util.NumberUtils;
import jcl.type.RandomStateType;
import org.apache.commons.lang3.RandomUtils;

/**
 * The {@link RandomStateStructImpl} is the object representation of a Lisp 'random-state' type.
 */
public final class RandomStateStructImpl extends BuiltInClassStruct implements RandomStateStruct {

	/**
	 * Public constructor.
	 */
	private RandomStateStructImpl() {
		super(RandomStateType.INSTANCE, null, null);
	}

	public static RandomStateStruct valueOf() {
		return new RandomStateStructImpl();
	}

	@Override
	public BigInteger randomInteger(final long limit) {
		final long randomLong = RandomUtils.nextLong(0L, limit);
		return BigInteger.valueOf(randomLong);
	}

	@Override
	public BigDecimal randomFloat(final double limit) {
		final double randomDouble = RandomUtils.nextDouble(0.0D, limit);
		return NumberUtils.bigDecimalValue(randomDouble);
	}
}
