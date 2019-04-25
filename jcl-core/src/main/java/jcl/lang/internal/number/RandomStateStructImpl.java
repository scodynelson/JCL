/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.internal.LispStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.RandomUtils;

/**
 * The {@link RandomStateStructImpl} is the object representation of a Lisp 'random-state' type.
 */
public final class RandomStateStructImpl extends LispStructImpl implements RandomStateStruct {

	/**
	 * Public constructor.
	 */
	private RandomStateStructImpl() {
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

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.RANDOM_STATE;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.RANDOM_STATE;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.RANDOM_STATE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.RANDOM_STATE) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
