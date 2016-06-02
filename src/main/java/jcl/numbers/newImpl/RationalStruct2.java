/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.math.BigInteger;

import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link RationalStruct2} is the object representation of a Lisp 'rational' type.
 */
public interface RationalStruct2 extends RealStruct2 {

	/**
	 * Returns numerator of this RationalStruct as an {@link IntegerStruct2}.
	 *
	 * @return the numerator value
	 */
	IntegerStruct2 numerator();

	/**
	 * Returns denominator of this RationalStruct as an {@link IntegerStruct2}.
	 *
	 * @return the denominator value
	 */
	IntegerStruct2 denominator();

	/**
	 * Returns a new RationalStruct with the provided {@code int} as a numerator. Because the default denominator value
	 * is {@code 1}, the result will be an {@link IntegerStruct2}.
	 *
	 * @param numerator
	 * 		the {@code int} numerator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided {@code int} as a numerator
	 */
	static RationalStruct2 valueOf(final Integer numerator) {
		return IntegerStruct2.valueOf(numerator);
	}

	/**
	 * Returns a new RationalStruct with the numerator and denominator values. If the denominator value is {@code 1},
	 * the result will be an {@link IntegerStruct2}.
	 *
	 * @param numerator
	 * 		the {@code int} numerator of the new RationalStruct
	 * @param denominator
	 * 		the {@code int} denominator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided numerator and denominator values
	 */
	static RationalStruct2 valueOf(final Integer numerator, final Integer denominator) {
//		if (denominator == 1) {
//			return IntegerStruct.valueOf(numerator);
//		}
//		final BigFraction bigFraction = new BigFraction(numerator, denominator);
//		return RatioStruct.valueOf(bigFraction);
		return null;
	}

	/**
	 * Returns a new RationalStruct with the provided {@code long} as a numerator. Because the default denominator
	 * value is {@code 1}, the result will be an {@link IntegerStruct2}.
	 *
	 * @param numerator
	 * 		the {@code long} numerator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided {@code long} as a numerator
	 */
	static RationalStruct2 valueOf(final Long numerator) {
		return IntegerStruct2.valueOf(numerator);
	}

	/**
	 * Returns a new RationalStruct with the numerator and denominator values. If the denominator value is {@code 1},
	 * the result will be an {@link IntegerStruct2}.
	 *
	 * @param numerator
	 * 		the {@code long} numerator of the new RationalStruct
	 * @param denominator
	 * 		the {@code long} denominator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided numerator and denominator values
	 */
	static RationalStruct2 valueOf(final Long numerator, final Long denominator) {
//		if (denominator == 1L) {
//			return IntegerStruct.valueOf(numerator);
//		}
//		final BigFraction bigFraction = new BigFraction(numerator, denominator);
//		return RatioStruct.valueOf(bigFraction);
		return null;
	}

	/**
	 * Returns a new RationalStruct with the provided {@link BigInteger} as a numerator. Because the default
	 * denominator
	 * value is {@code 1}, the result will be an {@link IntegerStruct2}.
	 *
	 * @param numerator
	 * 		the {@link BigInteger} numerator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided {@link BigInteger} as a numerator
	 */
	static RationalStruct2 valueOf(final BigInteger numerator) {
		return IntegerStruct2.valueOf(numerator);
	}

	/**
	 * Returns a new RationalStruct with the numerator and denominator values. If the denominator value is {@code 1},
	 * the result will be an {@link IntegerStruct2}.
	 *
	 * @param numerator
	 * 		the {@link BigInteger} numerator of the new RationalStruct
	 * @param denominator
	 * 		the {@link BigInteger} denominator of the new RationalStruct
	 *
	 * @return a new RationalStruct with the provided numerator and denominator values
	 */
	static RationalStruct2 valueOf(final BigInteger numerator, final BigInteger denominator) {
		if (BigInteger.ONE.equals(denominator)) {
			return IntegerStruct2.valueOf(numerator);
		}

		final Apint numeratorAp = new Apint(numerator);
		final Apint denominatorAp = new Apint(denominator);
		return RatioStruct2.valueOf(numeratorAp, denominatorAp);
	}

	static RationalStruct2 valueOf(final Aprational aprational) {
		if (aprational instanceof Apint) {
			return IntegerStruct2.valueOf((Apint) aprational);
		}
		return RatioStruct2.valueOf(aprational);
	}

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
	default NumberStruct2 signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return IntegerStruct2.ONE;
		} else {
			return IntegerStruct2.MINUS_ONE;
		}
	}

	@Override
	default RealStruct2 imagPart() {
		return IntegerStruct2.ZERO;
	}
}
