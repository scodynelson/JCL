/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import jcl.LispType;
import org.apfloat.Apcomplex;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

class RationalStruct2Impl<A extends Aprational> extends RealStruct2Impl<A> implements RationalStruct2 {

	RationalStruct2Impl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public IntegerStruct2 numerator() {
		final Apint numerator = ap.numerator();
		return IntegerStruct2.valueOf(numerator);
	}

	@Override
	public IntegerStruct2 denominator() {
		final Apint denominator = ap.denominator();
		return IntegerStruct2.valueOf(denominator);
	}

	/*
		RealStruct
	 */

	@Override
	public RationalStruct2 rational() {
		return this;
	}

	@Override
	public FloatStruct2 floatingPoint() {
		return FloatStruct2.valueOf(ap);
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return FloatStruct2.valueOf(ap, prototype);
	}

	@Override
	public RealStruct2 mod(final RealStruct2 divisor) {
		// TODO
		final QuotientRemainderResult2 floor = floor(divisor);
		return floor.getRemainder();
	}

	@Override
	public RealStruct2 rem(final RealStruct2 divisor) {
		// TODO
		final QuotientRemainderResult2 truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	/*
		NumberStruct
	 */

	@Override
	public Aprational ap() {
		return ap;
	}

	@Override
	public RationalStruct2 abs() {
		final Aprational abs = AprationalMath.abs(ap);
		return RationalStruct2.valueOf(abs);
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational add = ap.add((Aprational) numberAp);
			return RationalStruct2.valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational subtract = ap.subtract((Aprational) numberAp);
			return RationalStruct2.valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational multiply = ap.multiply((Aprational) numberAp);
			return RationalStruct2.valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational divide = ap.divide((Aprational) numberAp);
			return RationalStruct2.valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational rationalAp = (Aprational) numberAp;
			final boolean shouldReverseCompare = ap.preferCompare(rationalAp);
			return shouldReverseCompare ? rationalAp.equals(ap) : ap.equals(rationalAp);
		}
		return super.isEqualTo(number);
	}

	@Override
	public NumberStruct2 signum() {
		// TODO
		return super.signum();
	}

	@Override
	public RationalStruct2 realPart() {
		return this;
	}

	@Override
	public IntegerStruct2 imagPart() {
		return IntegerStruct2.ZERO;
	}

	@Override
	public RationalStruct2 conjugate() {
		return this;
	}

	@Override
	public RationalStruct2 negation() {
		final Aprational negate = ap.negate();
		return RationalStruct2.valueOf(negate);
	}

	@Override
	public RationalStruct2 reciprocal() {
		final Aprational reciprocal = new Aprational(ap.denominator(), ap.numerator());
		return RationalStruct2.valueOf(reciprocal);
	}
}
