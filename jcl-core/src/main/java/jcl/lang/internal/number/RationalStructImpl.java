/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.type.LispType;
import lombok.EqualsAndHashCode;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.apfloat.AprationalMath;

/**
 * Internal implementation class for {@link RationalStruct} objects.
 *
 * @param <A>
 * 		the type of {@link Aprational} the {@link RationalStruct} object will use for its value
 */
@EqualsAndHashCode(callSuper = true)
abstract class RationalStructImpl<A extends Aprational> extends RealStructImpl<A> implements RationalStruct {

	/**
	 * Package level constructor that passes the provided {@link LispType} and {@link A} {@link Aprational} value to the
	 * {@link RealStructImpl} superclass constructor.
	 *
	 * @param type
	 * 		the {@link LispType} of the {@link RationalStruct}
	 * @param ap
	 * 		the internal {@link Aprational} implementation value of the {@link RationalStruct}
	 */
	RationalStructImpl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public IntegerStruct numerator() {
		final Apint numerator = ap.numerator();
		return IntegerStructImpl.valueOf(numerator);
	}

	@Override
	public IntegerStruct denominator() {
		final Apint denominator = ap.denominator();
		return IntegerStructImpl.valueOf(denominator);
	}

	/*
		RealStruct
	 */

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public FloatStruct floatingPoint() {
		return FloatStructImpl.valueOf(ap);
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		return FloatStructImpl.valueOf(ap, prototype);
	}

	@Override
	protected RealStruct getRemainderReal(final RealStruct divisor, final Apfloat remainder) {
		if (divisor instanceof RationalStruct) {
			return RationalStruct.valueOf((Aprational) remainder);
		}
		return super.getRemainderReal(divisor, remainder);
	}

	/*
		NumberStruct
	 */

	@Override
	public Aprational ap() {
		return ap;
	}

	@Override
	public RationalStruct abs() {
		final Aprational abs = AprationalMath.abs(ap);
		return RationalStruct.valueOf(abs);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational add = ap.add((Aprational) numberAp);
			return RationalStruct.valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational subtract = ap.subtract((Aprational) numberAp);
			return RationalStruct.valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational multiply = ap.multiply((Aprational) numberAp);
			return RationalStruct.valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational divide = ap.divide((Aprational) numberAp);
			return RationalStruct.valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Aprational) {
			final Aprational rationalAp = (Aprational) numberAp;
			final boolean shouldReverseCompare = ap.preferCompare(rationalAp);
			return shouldReverseCompare ? rationalAp.equals(ap) : ap.equals(rationalAp);
		}
		return super.isEqualTo(number);
	}

	@Override
	public IntegerStruct signum() {
		final int signum = ap.signum();
		if (signum == 0) {
			return IntegerStruct.ZERO;
		}
		if (signum > 0) {
			return IntegerStruct.ONE;
		}
		return IntegerStruct.MINUS_ONE;
	}

	@Override
	public RationalStruct realPart() {
		return this;
	}

	@Override
	public IntegerStruct imagPart() {
		return IntegerStruct.ZERO;
	}

	@Override
	public RationalStruct conjugate() {
		return this;
	}

	@Override
	public RationalStruct negation() {
		final Aprational negate = ap.negate();
		return RationalStruct.valueOf(negate);
	}

	@Override
	public RationalStruct reciprocal() {
		final Aprational reciprocal = new Aprational(ap.denominator(), ap.numerator());
		return RationalStruct.valueOf(reciprocal);
	}
}
