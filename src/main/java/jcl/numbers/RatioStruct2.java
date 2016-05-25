/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import jcl.conditions.exceptions.DivisionByZeroException;
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

/**
 * The {@link RatioStruct2} is the object representation of a Lisp 'ratio' type.
 */
public final class RatioStruct2 extends InternalNumberStruct<Aprational> implements RationalStruct2 {

	/**
	 * Private constructor.
	 *
	 * @param aprational
	 * 		the value of the RatioStruct
	 */
	private RatioStruct2(final Aprational aprational) {
		super(RatioType.INSTANCE, aprational);
	}

	/**
	 * Returns a RatioStruct object with the provided {@link Aprational} value.
	 *
	 * @param aprational
	 * 		the {@link Aprational} value of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided {@link Aprational} value
	 */
	public static RatioStruct2 valueOf(final Aprational aprational) {
		return new RatioStruct2(aprational);
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
	public static RatioStruct2 valueOf(final Apint numerator, final Apint denominator) {
		final Aprational aprational = new Aprational(numerator, denominator);
		return new RatioStruct2(aprational);
	}

	/*
		RationalStruct
	 */

	@Override
	public IntegerStruct2 numerator() {
		return IntegerStruct2.valueOf(ap.numerator());
	}

	@Override
	public IntegerStruct2 denominator() {
		return IntegerStruct2.valueOf(ap.denominator());
	}

	/*
		RealStruct
	 */

	@Override
	public FloatStruct2 floatingPoint() {
		return FloatStruct2.valueOf(ap);
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return FloatStruct2.valueOf(ap, prototype);
	}

	private static RealStruct2 getRemainder(final Apfloat remainder, final RealStruct2 divisor) {
		if (divisor instanceof RationalStruct2) {
			// TODO: Aprational from Apfloat???
			return null;
		}
		return FloatStruct2.valueOf(remainder);
	}

	@Override
	public QuotientRemainderResult2 floor(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.floor();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(IntegerStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 ffloor(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.floor();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(FloatStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 ceiling(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.ceil();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(IntegerStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 fceiling(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.ceil();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(FloatStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 truncate(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.truncate();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(IntegerStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 ftruncate(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.truncate();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(FloatStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 round(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.floor();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(IntegerStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public QuotientRemainderResult2 fround(final RealStruct2 divisor) {
		final Apfloat divide = ap.divide(divisor.ap());

		final Apint quotient = divide.floor();
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);
		return new QuotientRemainderResult2(FloatStruct2.valueOf(quotient), getRemainder(remainder, divisor));
	}

	@Override
	public boolean isLessThan(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean preferCompare = ap.preferCompare(realAp);

		final int compareResult = preferCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult < 0;
	}

	@Override
	public boolean isGreaterThan(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean preferCompare = ap.preferCompare(realAp);

		final int compareResult = preferCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult <= 0;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean preferCompare = ap.preferCompare(realAp);

		final int compareResult = preferCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult > 0;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean preferCompare = ap.preferCompare(realAp);

		final int compareResult = preferCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult >= 0;
	}

	@Override
	public boolean plusp() {
		return Apcomplex.ZERO.compareTo(ap) > 0;
	}

	@Override
	public boolean minusp() {
		return Apcomplex.ZERO.compareTo(ap) < 0;
	}

	/*
		NumberStruct
	 */

	@Override
	public Aprational ap() {
		return ap;
	}

	@Override
	public boolean zerop() {
		return Apcomplex.ZERO.equals(ap);
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex add = ap.add(numberAp);
		return NumberStruct2.valueOf(add);
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = ap.subtract(numberAp);
		return NumberStruct2.valueOf(subtract);
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex multiply = ap.multiply(numberAp);
		return NumberStruct2.valueOf(multiply);
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = ap.divide(numberAp);
		return NumberStruct2.valueOf(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		return ap.equals(numberAp);
	}

	@Override
	public RealStruct2 abs() {
		return valueOf(AprationalMath.abs(ap));
	}

	@Override
	public RatioStruct2 negation() {
		return valueOf(ap.negate());
	}

	@Override
	public NumberStruct2 reciprocal() {
		final Apint numerator = ap.numerator();
		if (Apcomplex.ZERO.equals(numerator)) {
			throw new DivisionByZeroException("Division by zero.");
		}

		final Apint denominator = ap.denominator();
		if (Apcomplex.ONE.equals(numerator)) {
			return IntegerStruct2.valueOf(denominator);
		}
		// TODO: improve??
		return new RatioStruct2(new Aprational(denominator, numerator));
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
	public RealStruct2 sin() {
		return null;
	}

	@Override
	public RealStruct2 cos() {
		return null;
	}

	@Override
	public RealStruct2 tan() {
		return null;
	}

	@Override
	public RealStruct2 asin() {
		return null;
	}

	@Override
	public RealStruct2 acos() {
		return null;
	}

	@Override
	public RealStruct2 atan() {
		return null;
	}

	@Override
	public RealStruct2 sinh() {
		return null;
	}

	@Override
	public RealStruct2 cosh() {
		return null;
	}

	@Override
	public RealStruct2 tanh() {
		return null;
	}

	@Override
	public RealStruct2 asinh() {
		return null;
	}

	@Override
	public RealStruct2 acosh() {
		return null;
	}

	@Override
	public RealStruct2 atanh() {
		return null;
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(ap)
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
		return new EqualsBuilder().append(ap, rhs.ap)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return ap.toString();
	}
}
