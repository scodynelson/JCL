/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.math.BigInteger;

import jcl.lang.internal.RatioStructImpl;
import jcl.lang.internal.number.IntegerStructImpl;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public interface RationalStruct extends RealStruct {

	/**
	 * Returns the numerator of this RationalStruct.
	 *
	 * @return the numerator of this RationalStruct
	 */
	IntegerStruct numerator();

	/**
	 * Returns the denominator of this RationalStruct.
	 *
	 * @return the denominator of this RationalStruct
	 */
	IntegerStruct denominator();

	/**
	 * Returns the {@link BigFraction} representation of the RationalStruct.
	 *
	 * @return a {@link BigFraction} representation of the RationalStruct
	 */
	BigFraction toJavaBigFraction();

	/**
	 * Returns a RationalStruct object with the provided {@link BigFraction} value. If the denominator is equal to
	 * {@link BigInteger#ONE}, an {@link IntegerStruct} is returned; otherwise a {@link RatioStruct} is returned.
	 *
	 * @param fraction
	 * 		the {@link BigFraction} value representing data for the resulting RationalStruct
	 *
	 * @return a RationalStruct object with the provided {@link BigFraction} data values
	 */
	static RationalStruct toLispRational(final BigFraction fraction) {
		final BigFraction reducedFraction = fraction.reduce();
		if (BigInteger.ONE.compareTo(reducedFraction.getDenominator()) == 0) {
			return IntegerStruct.toLispInteger(reducedFraction.getNumerator());
		}
		return new RatioStructImpl(reducedFraction);
	}

	/**
	 * Returns a RationalStruct object with the provided numerator and denominator {@link BigInteger} values. If the
	 * denominator is equal to {@link BigInteger#ONE}, an {@link IntegerStruct} is returned; otherwise a {@link
	 * RatioStruct} is returned.
	 *
	 * @param numerator
	 * 		the {@link BigInteger} value of the numerator of the resulting RationalStruct
	 * @param denominator
	 * 		the {@link BigInteger} value of the denominator of the resulting RationalStruct
	 *
	 * @return a RationalStruct object with the provided numerator and denominator {@link BigInteger} values
	 */
	static RationalStruct toLispRational(final BigInteger numerator, final BigInteger denominator) {
		if (BigInteger.ONE.compareTo(denominator) == 0) {
			return IntegerStruct.toLispInteger(numerator);
		}
		return new RatioStructImpl(new BigFraction(numerator, denominator));
	}

	/**
	 * Returns a RationalStruct object with the provided numerator and denominator {@link IntegerStruct} values. If the
	 * denominator is equal to {@link IntegerStruct#ONE}, an {@link IntegerStruct} is returned; otherwise a {@link
	 * RatioStruct} is returned.
	 *
	 * @param numerator
	 * 		the {@link IntegerStruct} value of the numerator of the resulting RationalStruct
	 * @param denominator
	 * 		the {@link IntegerStruct} value of the denominator of the resulting RationalStruct
	 *
	 * @return a RationalStruct object with the provided numerator and denominator {@link IntegerStruct} values
	 */
	static RationalStruct toLispRational(final IntegerStruct numerator, final IntegerStruct denominator) {
		if (IntegerStruct.ONE.isEqualTo(denominator)) {
			return numerator;
		}
		return new RatioStructImpl(numerator, denominator);
	}

	/*
		RealStruct
	 */

	@Override
	default RationalStruct rational() {
		return this;
	}

	/*
		NumberStruct
	 */

	@Override
	Aprational ap();

	@Override
	IntegerStruct signum();

	@Override
	default RationalStruct realPart() {
		return this;
	}

	@Override
	default IntegerStruct imagPart() {
		return IntegerStruct.ZERO;
	}

	@Override
	default RationalStruct conjugate() {
		return this;
	}

	@Override
	RationalStruct negation();

	@Override
	RationalStruct reciprocal();

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof RationalStruct)
						&& ((RationalStruct) object).ap().equals(ap()));
	}

	/*
	DEPRECATED
	 */

	@Deprecated
	static RationalStruct valueOf(final Aprational aprational) {
		if (aprational instanceof Apint) {
			return IntegerStructImpl.valueOf((Apint) aprational);
		}
		return jcl.lang.internal.number.RatioStructImpl.valueOf(aprational);
	}

	@Deprecated
	static RationalStruct valueOf(final BigInteger numerator, final BigInteger denominator) {
		final Apint numeratorAp = new Apint(numerator);
		final Apint denominatorAp = new Apint(denominator);
		return valueOf(numeratorAp, denominatorAp);
	}

	@Deprecated
	static RationalStruct valueOf(final Apint numerator, final Apint denominator) {
		if (Apcomplex.ONE.equals(denominator)) {
			return IntegerStructImpl.valueOf(numerator);
		}
		return jcl.lang.internal.number.RatioStructImpl.valueOf(numerator, denominator);
	}
}
