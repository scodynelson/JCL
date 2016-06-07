/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import jcl.LispType;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;

/**
 * Internal implementation class for {@link NumberStruct2} objects.
 *
 * @param <A>
 * 		the type of {@link Apcomplex} the {@link NumberStruct2} object will use for its value
 */
abstract class NumberStruct2Impl<A extends Apcomplex> extends InternalNumberStruct<A> implements NumberStruct2 {

	/**
	 * Package level constructor that passes the provided {@link LispType} and {@link A} {@link Apcomplex} value to the
	 * {@link InternalNumberStruct} superclass constructor.
	 *
	 * @param type
	 * 		the {@link LispType} of the {@link NumberStruct2}
	 * @param ap
	 * 		the internal {@link Apcomplex} implementation value of the {@link NumberStruct2}
	 */
	NumberStruct2Impl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public Apcomplex ap() {
		return ap;
	}

	@Override
	public RealStruct2 abs() {
		final Apfloat abs = ApcomplexMath.abs(ap);
		return RealStruct2.valueOf(abs);
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
	public NumberStruct2 signum() {
		if (Apcomplex.ZERO.equals(ap)) {
			return this;
		}

		final Apfloat abs = ApcomplexMath.abs(ap);
		final Apcomplex signum = ap.divide(abs);
		return NumberStruct2.valueOf(signum);
	}

	@Override
	public RealStruct2 realPart() {
		final Apfloat real = ap.real();
		return RealStruct2.valueOf(real);
	}

	@Override
	public RealStruct2 imagPart() {
		final Apfloat imag = ap.imag();
		return RealStruct2.valueOf(imag);
	}

	@Override
	public NumberStruct2 conjugate() {
		final Apcomplex conj = ap.conj();
		return NumberStruct2.valueOf(conj);
	}

	@Override
	public NumberStruct2 negation() {
		final Apcomplex negate = ap.negate();
		return NumberStruct2.valueOf(negate);
	}

	@Override
	public NumberStruct2 reciprocal() {
		final Apcomplex reciprocal = Apcomplex.ONE.divide(ap);
		return NumberStruct2.valueOf(reciprocal);
	}

	@Override
	public NumberStruct2 exp() {
		final Apcomplex exp = ApcomplexMath.exp(ap);
		return NumberStruct2.valueOf(exp);
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return NumberStruct2.valueOf(pow);
	}

	@Override
	public NumberStruct2 log() {
		final Apcomplex log = ApcomplexMath.log(ap);
		return NumberStruct2.valueOf(log);
	}

	@Override
	public NumberStruct2 log(final NumberStruct2 base) {
		final Apcomplex baseAp = base.ap();
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return NumberStruct2.valueOf(log);
	}

	@Override
	public NumberStruct2 sqrt() {
		final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
		return NumberStruct2.valueOf(sqrt);
	}

	@Override
	public NumberStruct2 sin() {
		final Apcomplex sin = ApcomplexMath.sin(ap);
		return NumberStruct2.valueOf(sin);
	}

	@Override
	public NumberStruct2 cos() {
		final Apcomplex cos = ApcomplexMath.cos(ap);
		return NumberStruct2.valueOf(cos);
	}

	@Override
	public NumberStruct2 tan() {
		final Apcomplex tan = ApcomplexMath.tan(ap);
		return NumberStruct2.valueOf(tan);
	}

	@Override
	public NumberStruct2 asin() {
		final Apcomplex asin = ApcomplexMath.asin(ap);
		return NumberStruct2.valueOf(asin);
	}

	@Override
	public NumberStruct2 acos() {
		final Apcomplex acos = ApcomplexMath.acos(ap);
		return NumberStruct2.valueOf(acos);
	}

	@Override
	public NumberStruct2 atan() {
		final Apcomplex atan = ApcomplexMath.atan(ap);
		return NumberStruct2.valueOf(atan);
	}

	@Override
	public NumberStruct2 sinh() {
		final Apcomplex sinh = ApcomplexMath.sinh(ap);
		return NumberStruct2.valueOf(sinh);
	}

	@Override
	public NumberStruct2 cosh() {
		final Apcomplex cosh = ApcomplexMath.cosh(ap);
		return NumberStruct2.valueOf(cosh);
	}

	@Override
	public NumberStruct2 tanh() {
		final Apcomplex tanh = ApcomplexMath.tanh(ap);
		return NumberStruct2.valueOf(tanh);
	}

	@Override
	public NumberStruct2 asinh() {
		final Apcomplex asinh = ApcomplexMath.asinh(ap);
		return NumberStruct2.valueOf(asinh);
	}

	@Override
	public NumberStruct2 acosh() {
		final Apcomplex acosh = ApcomplexMath.acosh(ap);
		return NumberStruct2.valueOf(acosh);
	}

	@Override
	public NumberStruct2 atanh() {
		final Apcomplex atanh = ApcomplexMath.atanh(ap);
		return NumberStruct2.valueOf(atanh);
	}
}
