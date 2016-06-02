/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import jcl.types.RatioType;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

/**
 * The {@link RatioStruct2} is the object representation of a Lisp 'ratio' type.
 */
public final class RatioStruct2 extends RationalStruct2Impl<Aprational> {

	/**
	 * Private constructor.
	 *
	 * @param aprational
	 * 		the value of the RatioStruct2
	 */
	private RatioStruct2(final Aprational aprational) {
		super(RatioType.INSTANCE, aprational);
	}

	/**
	 * Returns a new RatioStruct2 representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new RatioStruct2
	 *
	 * @return a new RatioStruct2 representing the provided {@link String}
	 */
	public static RatioStruct2 valueOf(final String s) {
		final Aprational aprational = new Aprational(s);
		return valueOf(aprational);
	}

	/**
	 * Returns a RatioStruct2 object with the provided {@link Aprational} value.
	 *
	 * @param aprational
	 * 		the {@link Aprational} value of the resulting RatioStruct2
	 *
	 * @return a RatioStruct2 object with the provided {@link Aprational} value
	 */
	public static RatioStruct2 valueOf(final Aprational aprational) {
		return new RatioStruct2(aprational);
	}

	/**
	 * Returns a RatioStruct2 object with the provided numerator and denominator {@link Apint} values.
	 *
	 * @param numerator
	 * 		the {@link Apint} value of the numerator of the resulting RatioStruct2
	 * @param denominator
	 * 		the {@link Apint} value of the denominator of the resulting RatioStruct2
	 *
	 * @return a RatioStruct2 object with the provided numerator and denominator {@link Apint} values
	 */
	public static RatioStruct2 valueOf(final Apint numerator, final Apint denominator) {
		final Aprational aprational = new Aprational(numerator, denominator);
		return valueOf(aprational);
	}

	/*
		RealStruct
	 */

	@Override
	public RatioStruct2 rational() {
		return this;
	}

	/*
		NumberStruct
	 */

	@Override
	public RatioStruct2 abs() {
		final Aprational abs = AprationalMath.abs(ap);
		return valueOf(abs);
	}

	@Override
	public RatioStruct2 negation() {
		final Aprational negate = ap.negate();
		return valueOf(negate);
	}

	/*
		ToString
	 */

	@Override
	public String toString() {
		return ap.toString();
	}
}
