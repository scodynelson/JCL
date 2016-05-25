/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigInteger;

import jcl.conditions.exceptions.DivisionByZeroException;
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Aprational;

/**
 * The {@link RatioStruct2} is the object representation of a Lisp 'ratio' type.
 */
public final class RatioStruct2 extends InternalNumberStruct<Aprational> implements RationalStruct2 {

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	final BigFraction bigFraction;

	private final Aprational aprational;

	/**
	 * Private constructor.
	 *
	 * @param bigFraction
	 * 		the value of the RatioStruct
	 */
	private RatioStruct2(final BigFraction bigFraction) {
		super(RatioType.INSTANCE, null);
		this.bigFraction = bigFraction;

		this.aprational = null;
	}

	public Aprational getAp() {
		return aprational;
	}

	/**
	 * Returns a RatioStruct object with the provided {@link BigFraction} value.
	 *
	 * @param bigFraction
	 * 		the {@link BigFraction} value of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided {@link BigFraction} value
	 */
	public static RatioStruct2 valueOf(final BigFraction bigFraction) {
		return new RatioStruct2(bigFraction);
	}

	/**
	 * Returns a RatioStruct object with the provided numerator and denominator {@link BigFraction} values.
	 *
	 * @param numerator
	 * 		the {@link BigInteger} value of the numerator of the resulting RatioStruct
	 * @param denominator
	 * 		the {@link BigInteger} value of the denominator of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided numerator and denominator {@link BigFraction} values
	 */
	public static RatioStruct2 valueOf(final BigInteger numerator, final BigInteger denominator) {
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return new RatioStruct2(bigFraction);
	}

	/**
	 * Getter for ratio {@link #bigFraction} property.
	 *
	 * @return ratio {@link #bigFraction} property
	 */
	@Deprecated
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	/*
		RationalStruct
	 */

	@Override
	public IntegerStruct2 numerator() {
		return IntegerStruct2.valueOf(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct2 denominator() {
		return IntegerStruct2.valueOf(bigFraction.getDenominator());
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct2 floatingPoint() {
//		return SingleFloatStruct.valueOf(bigFraction.floatValue());
		return null;
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return null;
	}

	@Override
	public RealStruct2 mod(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public RealStruct2 rem(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public ComplexStruct cis() {
		return null;
	}

	@Override
	public QuotientRemainderResult floor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult ffloor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult ceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult fceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult truncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult ftruncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult round(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult fround(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public boolean isLessThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean plusp() {
		return BigFraction.ZERO.compareTo(bigFraction) > 0;
	}

	@Override
	public boolean minusp() {
		return BigFraction.ZERO.compareTo(bigFraction) < 0;
	}

	/*
		NumberStruct
	 */

	@Override
	public boolean zerop() {
		return BigFraction.ZERO.compareTo(bigFraction) == 0;
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		return false;
	}

	@Override
	public RealStruct2 abs() {
		final BigInteger numerator = bigFraction.getNumerator();
		if (numerator.signum() >= 0) {
			return this;
		}
		return negation();
	}

	@Override
	public RatioStruct2 negation() {
		return valueOf(bigFraction.negate());
	}

	@Override
	public NumberStruct2 reciprocal() {
		final BigInteger numerator = bigFraction.getNumerator();
		if (BigInteger.ZERO.equals(numerator)) {
			throw new DivisionByZeroException("Division by zero.");
		}

		final BigInteger denominator = bigFraction.getDenominator();
		if (BigInteger.ONE.equals(numerator)) {
			return IntegerStruct2.valueOf(denominator);
		}
		return new RatioStruct2(bigFraction.reciprocal());
	}

	@Override
	public NumberStruct2 exp() {
		return null;
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
		return null;
	}

	@Override
	public NumberStruct2 log() {
		return null;
	}

	@Override
	public NumberStruct2 sqrt() {
		return null;
	}

	@Override
	public NumberStruct2 sin() {
		return null;
	}

	@Override
	public NumberStruct2 cos() {
		return null;
	}

	@Override
	public NumberStruct2 tan() {
		return null;
	}

	@Override
	public NumberStruct2 asin() {
		return null;
	}

	@Override
	public NumberStruct2 acos() {
		return null;
	}

	@Override
	public NumberStruct2 atan() {
		return null;
	}

	@Override
	public NumberStruct2 sinh() {
		return null;
	}

	@Override
	public NumberStruct2 cosh() {
		return null;
	}

	@Override
	public NumberStruct2 tanh() {
		return null;
	}

	@Override
	public NumberStruct2 asinh() {
		return null;
	}

	@Override
	public NumberStruct2 acosh() {
		return null;
	}

	@Override
	public NumberStruct2 atanh() {
		return null;
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(bigFraction)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final RatioStruct2 rhs = (RatioStruct2) obj;
		return new EqualsBuilder().append(bigFraction, rhs.bigFraction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return bigFraction.toString();
	}
}
