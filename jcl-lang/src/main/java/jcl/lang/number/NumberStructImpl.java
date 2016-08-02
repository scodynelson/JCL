/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.type.LispType;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;

/**
 * Internal implementation class for {@link NumberStruct} objects.
 *
 * @param <A>
 * 		the type of {@link Apcomplex} the {@link NumberStruct} object will use for its value
 */
abstract class NumberStructImpl<A extends Apcomplex> extends InternalNumberStruct<A> implements NumberStruct {

	/**
	 * Package level constructor that passes the provided {@link LispType} and {@link A} {@link Apcomplex} value to the
	 * {@link InternalNumberStruct} superclass constructor.
	 *
	 * @param type
	 * 		the {@link LispType} of the {@link NumberStruct}
	 * @param ap
	 * 		the internal {@link Apcomplex} implementation value of the {@link NumberStruct}
	 */
	NumberStructImpl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public Apcomplex ap() {
		return ap;
	}

	@Override
	public RealStruct abs() {
		final Apfloat abs = ApcomplexMath.abs(ap);
		return RealStruct.valueOf(abs);
	}

	@Override
	public boolean zerop() {
		return Apcomplex.ZERO.equals(ap);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex add = ap.add(numberAp);
		return NumberStruct.valueOf(add);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = ap.subtract(numberAp);
		return NumberStruct.valueOf(subtract);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex multiply = ap.multiply(numberAp);
		return NumberStruct.valueOf(multiply);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = ap.divide(numberAp);
		return NumberStruct.valueOf(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		return ap.equals(numberAp);
	}

	@Override
	public NumberStruct signum() {
		if (Apcomplex.ZERO.equals(ap)) {
			return this;
		}

		final Apfloat abs = ApcomplexMath.abs(ap);
		final Apcomplex signum = ap.divide(abs);
		return NumberStruct.valueOf(signum);
	}

	@Override
	public RealStruct realPart() {
		final Apfloat real = ap.real();
		return RealStruct.valueOf(real);
	}

	@Override
	public RealStruct imagPart() {
		final Apfloat imag = ap.imag();
		return RealStruct.valueOf(imag);
	}

	@Override
	public NumberStruct conjugate() {
		final Apcomplex conj = ap.conj();
		return NumberStruct.valueOf(conj);
	}

	@Override
	public NumberStruct negation() {
		final Apcomplex negate = ap.negate();
		return NumberStruct.valueOf(negate);
	}

	@Override
	public NumberStruct reciprocal() {
		final Apcomplex reciprocal = Apcomplex.ONE.divide(ap);
		return NumberStruct.valueOf(reciprocal);
	}

	@Override
	public NumberStruct exp() {
		final Apcomplex exp = ApcomplexMath.exp(ap);
		return NumberStruct.valueOf(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return NumberStruct.valueOf(pow);
	}

	@Override
	public NumberStruct log() {
		final Apcomplex log = ApcomplexMath.log(ap);
		return NumberStruct.valueOf(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Apcomplex baseAp = base.ap();
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return NumberStruct.valueOf(log);
	}

	@Override
	public NumberStruct sqrt() {
		final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
		return NumberStruct.valueOf(sqrt);
	}

	@Override
	public NumberStruct sin() {
		final Apcomplex sin = ApcomplexMath.sin(ap);
		return NumberStruct.valueOf(sin);
	}

	@Override
	public NumberStruct cos() {
		final Apcomplex cos = ApcomplexMath.cos(ap);
		return NumberStruct.valueOf(cos);
	}

	@Override
	public NumberStruct tan() {
		final Apcomplex tan = ApcomplexMath.tan(ap);
		return NumberStruct.valueOf(tan);
	}

	@Override
	public NumberStruct asin() {
		final Apcomplex asin = ApcomplexMath.asin(ap);
		return NumberStruct.valueOf(asin);
	}

	@Override
	public NumberStruct acos() {
		final Apcomplex acos = ApcomplexMath.acos(ap);
		return NumberStruct.valueOf(acos);
	}

	@Override
	public NumberStruct atan() {
		final Apcomplex atan = ApcomplexMath.atan(ap);
		return NumberStruct.valueOf(atan);
	}

	@Override
	public NumberStruct sinh() {
		final Apcomplex sinh = ApcomplexMath.sinh(ap);
		return NumberStruct.valueOf(sinh);
	}

	@Override
	public NumberStruct cosh() {
		final Apcomplex cosh = ApcomplexMath.cosh(ap);
		return NumberStruct.valueOf(cosh);
	}

	@Override
	public NumberStruct tanh() {
		final Apcomplex tanh = ApcomplexMath.tanh(ap);
		return NumberStruct.valueOf(tanh);
	}

	@Override
	public NumberStruct asinh() {
		final Apcomplex asinh = ApcomplexMath.asinh(ap);
		return NumberStruct.valueOf(asinh);
	}

	@Override
	public NumberStruct acosh() {
		final Apcomplex acosh = ApcomplexMath.acosh(ap);
		return NumberStruct.valueOf(acosh);
	}

	@Override
	public NumberStruct atanh() {
		final Apcomplex atanh = ApcomplexMath.atanh(ap);
		return NumberStruct.valueOf(atanh);
	}
}
