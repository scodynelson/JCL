/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.math.BigInteger;

import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.internal.number.RatioStructImpl;
import org.apfloat.Apcomplex;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link RationalStruct} is the object representation of a Lisp 'rational' type.
 */
public interface RationalStruct extends RealStruct {

	/**
	 * Returns a RationalStruct from the provided {@link Aprational} value. If the {@link Aprational} is an {@link
	 * Apint}, {@link IntegerStructImpl#valueOf(Apint)} is invoked to create the appropriate {@link IntegerStruct}
	 * instead. Otherwise, {@link RatioStructImpl#valueOf(Aprational)} is invoked to create the appropriate {@link
	 * RatioStruct}
	 *
	 * @param aprational
	 * 		the {@link Aprational} to be used as the value of the resulting RationalStruct
	 *
	 * @return a RationalStruct with the provided {@link Aprational} as its value
	 */
	static RationalStruct valueOf(final Aprational aprational) {
		if (aprational instanceof Apint) {
			return IntegerStructImpl.valueOf((Apint) aprational);
		}
		return RatioStructImpl.valueOf(aprational);
	}

	/**
	 * Returns a RationalStruct object with the provided numerator and denominator {@link Apint} values. If the
	 * denominator is equal to {@link Apcomplex#ONE}, an {@link IntegerStruct} is returned; otherwise a {@link
	 * RatioStruct} is returned.
	 *
	 * @param numerator
	 * 		the {@link Apint} value of the numerator of the resulting RationalStruct
	 * @param denominator
	 * 		the {@link Apint} value of the denominator of the resulting RationalStruct
	 *
	 * @return a RationalStruct object with the provided numerator and denominator {@link Apint} values
	 */
	static RationalStruct valueOf(final BigInteger numerator, final BigInteger denominator) {
		final Apint numeratorAp = new Apint(numerator);
		final Apint denominatorAp = new Apint(denominator);
		return valueOf(numeratorAp, denominatorAp);
	}

	/**
	 * Returns a RationalStruct object with the provided numerator and denominator {@link Apint} values. If the
	 * denominator is equal to {@link Apcomplex#ONE}, an {@link IntegerStruct} is returned; otherwise a {@link
	 * RatioStruct} is returned.
	 *
	 * @param numerator
	 * 		the {@link Apint} value of the numerator of the resulting RationalStruct
	 * @param denominator
	 * 		the {@link Apint} value of the denominator of the resulting RationalStruct
	 *
	 * @return a RationalStruct object with the provided numerator and denominator {@link Apint} values
	 */
	static RationalStruct valueOf(final Apint numerator, final Apint denominator) {
		if (Apcomplex.ONE.equals(denominator)) {
			return IntegerStructImpl.valueOf(numerator);
		}
		return RatioStructImpl.valueOf(numerator, denominator);
	}

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
}
