/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link RationalStruct2} is the object representation of a Lisp 'rational' type.
 */
public interface RationalStruct2 extends RealStruct2 {

	/**
	 * Returns a RationalStruct2 from the provided {@link Aprational} value. If the {@link Aprational} is an {@link
	 * Apint}, {@link IntegerStruct2#valueOf(Apint)} is invoked to create the appropriate {@link IntegerStruct2}
	 * instead. Otherwise, {@link RatioStruct2#valueOf(Aprational)} is invoked to create the appropriate {@link
	 * RatioStruct2}
	 *
	 * @param aprational
	 * 		the {@link Aprational} to be used as the value of the resulting RationalStruct2
	 *
	 * @return a RationalStruct2 with the provided {@link Aprational} as its value
	 */
	static RationalStruct2 valueOf(final Aprational aprational) {
		if (aprational instanceof Apint) {
			return IntegerStruct2.valueOf((Apint) aprational);
		}
		return RatioStruct2.valueOf(aprational);
	}

	/**
	 * Returns the numerator of this RationalStruct2.
	 *
	 * @return the numerator of this RationalStruct2
	 */
	IntegerStruct2 numerator();

	/**
	 * Returns the denominator of this RationalStruct2.
	 *
	 * @return the denominator of this RationalStruct2
	 */
	IntegerStruct2 denominator();

	/*
		RealStruct
	 */

	@Override
	default RationalStruct2 rational() {
		return this;
	}

	/*
		NumberStruct
	 */

	@Override
	Aprational ap();

	@Override
	IntegerStruct2 signum();

	@Override
	default RationalStruct2 realPart() {
		return this;
	}

	@Override
	default IntegerStruct2 imagPart() {
		return IntegerStruct2.ZERO;
	}

	@Override
	default RationalStruct2 conjugate() {
		return this;
	}

	@Override
	RationalStruct2 negation();

	@Override
	RationalStruct2 reciprocal();
}
