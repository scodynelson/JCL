/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import jcl.lang.RatioStruct;
import jcl.type.RatioType;
import lombok.EqualsAndHashCode;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

/**
 * The {@link RatioStructImpl} is the object representation of a Lisp 'ratio' type.
 */
@EqualsAndHashCode(callSuper = true)
public final class RatioStructImpl extends RationalStructImpl<Aprational> implements RatioStruct {

	/**
	 * Private constructor.
	 *
	 * @param aprational
	 * 		the value of the RatioStruct
	 */
	private RatioStructImpl(final Aprational aprational) {
		super(RatioType.INSTANCE, aprational);
	}

	/**
	 * Returns a new RatioStruct representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new RatioStruct
	 *
	 * @return a new RatioStruct representing the provided {@link String}
	 */
	public static RatioStruct valueOf(final String s) {
		final Aprational aprational = new Aprational(s);
		return valueOf(aprational);
	}

	/**
	 * Returns a RatioStruct object with the provided {@link Aprational} value.
	 *
	 * @param aprational
	 * 		the {@link Aprational} value of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided {@link Aprational} value
	 */
	public static RatioStruct valueOf(final Aprational aprational) {
		return new RatioStructImpl(aprational);
	}

	/**
	 * Returns a RatioStruct object with the provided numerator and denominator {@link Apint} values.
	 *
	 * @param numerator
	 * 		the {@link Apint} value of the numerator of the resulting RatioStruct
	 * @param denominator
	 * 		the {@link Apint} value of the denominator of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided numerator and denominator {@link Apint} values
	 */
	public static RatioStruct valueOf(final Apint numerator, final Apint denominator) {
		final Aprational aprational = new Aprational(numerator, denominator);
		return valueOf(aprational);
	}

	/*
		RealStruct
	 */

	@Override
	public RatioStruct rational() {
		return this;
	}

	/*
		NumberStruct
	 */

	@Override
	public RatioStruct abs() {
		final Aprational abs = AprationalMath.abs(ap);
		return valueOf(abs);
	}

	@Override
	public RatioStruct realPart() {
		return this;
	}

	@Override
	public RatioStruct conjugate() {
		return this;
	}

	@Override
	public RatioStruct negation() {
		final Aprational negate = ap.negate();
		return valueOf(negate);
	}

	/*
		ToString
	 */

	@Override
	public String toString() {
		return ap.toString(true);
	}
}
