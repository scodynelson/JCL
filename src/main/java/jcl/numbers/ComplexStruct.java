/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.ComplexType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7848008215064899579L;

	public static final ComplexStruct I = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);

	public static final ComplexStruct ZERO = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ZERO);

	public static final ComplexStruct ONE = new ComplexStruct(IntegerStruct.ONE, IntegerStruct.ZERO);

	/**
	 * The {@link RealStruct} that comprises the real value of the complex.
	 */
	private final RealStruct real;

	/**
	 * The {@link RealStruct} that comprises the imaginary value of the complex.
	 */
	private final RealStruct imaginary;

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link RealStruct} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link RealStruct} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RealStruct real, final RealStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.imaginary = imaginary;

		final BigInteger realWithScale = real.getBigInteger().multiply(BigInteger.TEN);
		this.real = new FloatStruct(new BigDecimal(realWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;

		final BigInteger imaginaryWithScale = imaginary.getBigInteger().multiply(BigInteger.TEN);
		this.imaginary = new FloatStruct(new BigDecimal(imaginaryWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = new FloatStruct(imaginary.getBigFraction().bigDecimalValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = new FloatStruct(real.getBigFraction().bigDecimalValue());
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct getReal() {
		return real;
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct getImaginary() {
		return imaginary;
	}

	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	public boolean equalp(final LispStruct lispStruct) {
		return isEqualTo(lispStruct);
	}

	@Override
	public RealStruct abs() {
		if (real.zerop()) {
			return imaginary.abs();
		}

		final BigDecimal bdReal = real.bigDecimalValue();
		final BigDecimal bdImag = imaginary.bigDecimalValue();

		// TODO: can we avoid possible precision loss here???
		final double dblReal = bdReal.doubleValue();
		final double dblImag = bdImag.doubleValue();
		return new FloatStruct(BigDecimal.valueOf(StrictMath.hypot(dblReal, dblImag)));
	}

	@Override
	public boolean zerop() {
		return real.zerop() && imaginary.zerop();
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		}

		final RealStruct abs = abs();
		return divide(abs);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) real.add(c.real), (RealStruct) imaginary.add(c.imaginary));
		}
		return new ComplexStruct((RealStruct) real.add(number), imaginary);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) real.subtract(c.real), (RealStruct) imaginary.subtract(c.imaginary));
		}
		return new ComplexStruct((RealStruct) real.subtract(number), imaginary);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final NumberStruct a = real;
			final NumberStruct b = imaginary;
			final NumberStruct c = ((ComplexStruct) number).real;
			final NumberStruct d = ((ComplexStruct) number).imaginary;
			// xy = (ac - bd) + i(ad + bc)
			// real part = ac - bd
			// imag part = ad + bc
			final NumberStruct ac = a.multiply(c);
			final NumberStruct bd = b.multiply(d);
			final NumberStruct ad = a.multiply(d);
			final NumberStruct bc = b.multiply(c);
			return new ComplexStruct((RealStruct) ac.subtract(bd), (RealStruct) ad.add(bc));
		}
		return new ComplexStruct((RealStruct) real.multiply(number), (RealStruct) imaginary.multiply(number));
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final NumberStruct a = real;
			final NumberStruct b = imaginary;
			final NumberStruct c = ((ComplexStruct) number).real;
			final NumberStruct d = ((ComplexStruct) number).imaginary;
			final NumberStruct ac = a.multiply(c);
			final NumberStruct bd = b.multiply(d);
			final NumberStruct bc = b.multiply(c);
			final NumberStruct ad = a.multiply(d);
			final NumberStruct denominator = c.multiply(c).add(d.multiply(d));
			return new ComplexStruct((RealStruct) ac.add(bd).divide(denominator), (RealStruct) bc.subtract(ad).divide(denominator));
		}
		return new ComplexStruct((RealStruct) real.divide(number), (RealStruct) imaginary.divide(number));
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) obj;
			return real.isEqualTo(c.real) && imaginary.isEqualTo(c.imaginary);
		}
		if (obj instanceof NumberStruct) {
			// obj is a number, but not complex.
			if (imaginary instanceof FloatStruct) {
				if (((FloatStruct) imaginary).getBigDecimal().compareTo(BigDecimal.ZERO) == 0) {
					if (obj instanceof IntegerStruct) {
						return new BigDecimal(((IntegerStruct) obj).getBigInteger()).compareTo(((FloatStruct) real).getBigDecimal()) == 0;
					}
					if (obj instanceof FloatStruct) {
						return ((FloatStruct) obj).getBigDecimal().compareTo(((FloatStruct) real).getBigDecimal()) == 0;
					}
				}
			}
			return false;
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isNotEqualTo(final LispStruct obj) {
		return !isEqualTo(obj);
	}

	@Override
	public NumberStruct exp() {

//		double expReal = FastMath.exp(real);
//		return createComplex(expReal *  FastMath.cos(imaginary),
//				expReal * FastMath.sin(imaginary));
		return real.exp().multiply(imaginary.cis());
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				if (real instanceof FloatStruct) {
					return ONE;
				}
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}
		if (zerop()) {
			return this;
		}
		if (isEqualTo(ONE)) {
			return this;
		}

		if ((power instanceof IntegerStruct) && (real instanceof RationalStruct)) {
			// exact math version
			return intexp(this, (IntegerStruct) power);
		}
		// for anything not a rational or complex rational, use
		// float approximation.
		boolean wantDoubleFloat = false;
		if ((power instanceof FloatStruct)
				|| ((real instanceof FloatStruct) || (imaginary instanceof FloatStruct))
				|| ((power instanceof ComplexStruct)
				&& ((((ComplexStruct) power).real instanceof FloatStruct)
				|| (((ComplexStruct) power).imaginary instanceof FloatStruct)))) {
			wantDoubleFloat = true;
		}

		final NumberStruct base;
		NumberStruct newPower = power;
		if (wantDoubleFloat) {
			if (newPower instanceof ComplexStruct) {
				final ComplexStruct powerComplex = (ComplexStruct) newPower;
				newPower = new ComplexStruct(
						new FloatStruct(BigDecimal.valueOf(powerComplex.real.doubleValue())),
						new FloatStruct(BigDecimal.valueOf(powerComplex.imaginary.doubleValue())));
			} else {
				final RealStruct powerReal = (RealStruct) newPower;
				newPower = new FloatStruct(BigDecimal.valueOf(powerReal.doubleValue()));
			}

			base = this;
		} else {
			base = this;
		}

		return newPower.multiply(base.log()).exp();
	}

	@Override
	public NumberStruct sqrt() {
		final RealStruct imagpart = imaginary;
		if (imagpart.zerop()) {
			final RealStruct realpart = real;
			if (realpart.minusp()) {
				return new ComplexStruct(imagpart, (RealStruct) IntegerStruct.ZERO.subtract(realpart).sqrt());
			} else {
				return new ComplexStruct((RealStruct) realpart.sqrt(), imagpart);
			}
		}

//		if (real == 0.0 && imaginary == 0.0) {
//			return createComplex(0.0, 0.0);
//		}
//
//		double t = FastMath.sqrt((FastMath.abs(real) + abs()) / 2.0);
//		if (real >= 0.0) {
//			return createComplex(t, imaginary / (2.0 * t));
//		} else {
//			return createComplex(FastMath.abs(imaginary) / (2.0 * t),
//					FastMath.copySign(1d, imaginary) * t);
//		}
		return log().divide(IntegerStruct.TWO).exp();
	}

	@Override
	public NumberStruct log() {
		// TODO: check complex results when item is not complex!!!
		// TODO: no casting!!!
		final FloatStruct re = (FloatStruct) real;
		final FloatStruct im = (FloatStruct) imaginary;
		final FloatStruct phase = new FloatStruct(BigDecimal.valueOf(FastMath.atan2(im.getBigDecimal().doubleValue(), re.getBigDecimal().doubleValue())));  // atan(y/x)
		final FloatStruct abs = (FloatStruct) abs();

//		return createComplex(FastMath.log(abs()),
//				FastMath.atan2(imaginary, real));
		return new ComplexStruct(new FloatStruct(BigDecimal.valueOf(FastMath.log(abs.getBigDecimal().doubleValue()))), phase);
	}

	@Override
	public NumberStruct sin() {
		final NumberStruct n = multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE));
		NumberStruct result = n.exp();
		result = result.subtract(n.multiply(IntegerStruct.MINUS_ONE).exp());

//		return createComplex(FastMath.sin(real) * FastMath.cosh(imaginary),
//				FastMath.cos(real) * FastMath.sinh(imaginary));
		return result.divide(IntegerStruct.TWO.multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE)));
	}

	@Override
	public NumberStruct cos() {
		final NumberStruct n = multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE));
		NumberStruct result = n.exp();
		result = result.add(n.multiply(IntegerStruct.MINUS_ONE).exp());

//		return createComplex(FastMath.cos(real) * FastMath.cosh(imaginary),
//				-FastMath.sin(real) * FastMath.sinh(imaginary));
		return result.divide(IntegerStruct.TWO);
	}

	@Override
	public NumberStruct tan() {

//		double real2 = 2.0 * real;
//		double imaginary2 = 2.0 * imaginary;
//		double d = FastMath.cos(real2) + FastMath.cosh(imaginary2);
//
//		return createComplex(FastMath.sin(real2) / d,
//				FastMath.sinh(imaginary2) / d);
		return sin().divide(cos());
	}

	@Override
	public NumberStruct asin() {
		NumberStruct result = multiply(this);
		result = IntegerStruct.ONE.subtract(result);
		result = result.sqrt();
		NumberStruct n = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);
		n = n.multiply(this);
		result = n.add(result);
		result = result.log();
		result = result.multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.MINUS_ONE));

//		return sqrt1z().add(this.multiply(I)).log().multiply(I.negate());
		return result;
	}

	@Override
	public NumberStruct acos() {
		NumberStruct result = new FloatStruct(new BigDecimal(Math.PI / 2));
		result = result.subtract(asin());

//		return this.add(this.sqrt1z().multiply(I)).log().multiply(I.negate());
		return result;
	}

//	public Complex sqrt1z() {
//		return createComplex(1.0, 0.0).subtract(this.multiply(this)).sqrt();
//	}

	@Override
	public NumberStruct atan() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.atan(), im);
		}
		NumberStruct result = multiply(this);
		result = result.add(IntegerStruct.ONE);
		result = IntegerStruct.ONE.divide(result);
		result = result.sqrt();
		NumberStruct n = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);
		n = n.multiply(this);
		n = n.add(IntegerStruct.ONE);
		result = n.multiply(result);
		result = result.log();
		result = result.multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.MINUS_ONE));

//		return this.add(I).divide(I.subtract(this)).log()
//		           .multiply(I.divide(createComplex(2.0, 0.0)));
		return result;
	}

	@Override
	public NumberStruct sinh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.sinh(), im);
		}
		NumberStruct result = exp();
		result = result.subtract(multiply(IntegerStruct.MINUS_ONE).exp());
		result = result.divide(IntegerStruct.TWO);

//		return createComplex(FastMath.sinh(real) * FastMath.cos(imaginary),
//				FastMath.cosh(real) * FastMath.sin(imaginary));
		return result;
	}

	@Override
	public NumberStruct cosh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.cosh(), im);
		}
		NumberStruct result = exp();
		result = result.add(multiply(IntegerStruct.MINUS_ONE).exp());
		result = result.divide(IntegerStruct.TWO);

//		return createComplex(FastMath.cosh(real) * FastMath.cos(imaginary),
//				FastMath.sinh(real) * FastMath.sin(imaginary));
		return result;
	}

	@Override
	public NumberStruct tanh() {

//		double real2 = 2.0 * real;
//		double imaginary2 = 2.0 * imaginary;
//		double d = FastMath.cosh(real2) + FastMath.cos(imaginary2);
//
//		return createComplex(FastMath.sinh(real2) / d,
//				FastMath.sin(imaginary2) / d);
		return sinh().divide(cosh());
	}

	@Override
	public NumberStruct asinh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.asinh(), im);
		}
		NumberStruct result = multiply(this);
		result = IntegerStruct.ONE.add(result);
		result = result.sqrt();
		result = result.add(this);
		result = result.log();

		// asinh(z) = i*asin(-i*z)
//		ComplexStruct miz = new ComplexStruct(this.imaginary, -this.real);
//		ComplexStruct result = asin(miz);
//		double rx = result.imaginary;
//		result.imaginary = result.real;
//		result.real = -rx;
//		return result;

		//public static final ComplexStruct MINUS_I=new ComplexStruct(0.0,-1.0);
		//public static final ComplexStruct PI_2_I=new ComplexStruct(0.0,Math.PI/2.0);
		//public static final ComplexStruct MINUS_PI_2_I=new ComplexStruct(0.0,-Math.PI/2.0);
//		if(this.equals(I))
//			return PI_2_I;
//		else if(this.equals(MINUS_I))
//			return MINUS_PI_2_I;
//		else {
//			// log(z+sqrt(z*z+1))
//			final ComplexStruct root=sqrt(this.real*this.real-this.imaginary*this.imaginary+1.0,2.0*this.real*this.imaginary);
//			return log(this.real+root.real,this.imaginary+root.imaginary);
//		}
		return result;
	}

	@Override
	public NumberStruct acosh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.acosh(), im);
		}
		NumberStruct n1 = add(IntegerStruct.ONE);
		n1 = n1.divide(IntegerStruct.TWO);
		n1 = n1.sqrt();
		NumberStruct n2 = subtract(IntegerStruct.ONE);
		n2 = n2.divide(IntegerStruct.TWO);
		n2 = n2.sqrt();
		NumberStruct result = n1.add(n2);
		result = result.log();
		result = result.multiply(IntegerStruct.TWO);

		// private final static long negZeroBits = 0x8000000000000000L;
//		ComplexStruct result = acos(this);
//		double rx = -result.imaginary;
//		result.imaginary = result.real;
//		result.real = rx;
//		boolean isNegZero = (Double.doubleToLongBits(result.real) == negZeroBits);
//		if (result.real < 0.0 || isNegZero) {
//			result.real = -result.real;
//			result.imaginary = -result.imaginary;
//		}
//		return result;

		//public static final ComplexStruct MINUS_ONE=new ComplexStruct(-1.0,0.0);
		//public static final ComplexStruct PI_I=new ComplexStruct(0.0,Math.PI);
//		if(this.equals(ONE))
//			return ZERO;
//		else if(this.equals(MINUS_ONE))
//			return PI_I;
//		else {
//			// log(z+sqrt(z*z-1))
//			final ComplexStruct root=sqrt(this.real*this.real-this.imaginary*this.imaginary-1.0,2.0*this.real*this.imaginary);
//			return log(this.real+root.real,this.imaginary+root.imaginary);
//		}
		return result;
	}

	@Override
	public NumberStruct atanh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.atanh(), im);
		}

		final NumberStruct n1 = IntegerStruct.ONE.add(this).log();
		final NumberStruct n2 = IntegerStruct.ONE.subtract(this).log();
		NumberStruct result = n1.subtract(n2);
		result = result.divide(IntegerStruct.TWO);

		// atanh(z) = i*atan(-i*z)
//		ComplexStruct miz = new ComplexStruct(this.imaginary, -this.real);
//		ComplexStruct result = atan(miz);
//		double rx = result.imaginary;
//		result.imaginary = result.real;
//		result.real = -rx;
//		return result;

		// 1/2 log((1+z)/(1-z))
//		final double modSqr=this.modSqr();
//		final double denom=1.0+modSqr-2.0*this.real;
//		return log_2((1.0-modSqr)/denom,2.0*this.imaginary/denom);
		return result;
	}

//	public double modSqr() {
//		return real*real+imaginary*imaginary;
//	}
//	private final static ComplexStruct log_2(final double real,final double imag) {
//		return new ComplexStruct(Math.log(mod(real,imag))/2.0,arg(real,imag)/2.0);
//	}
//	private static double arg(final double real,final double imag) {
//		return Math.atan2(imag,real);
//	}
//	private static double mod(final double real,final double imag) {
//		final double reAbs=Math.abs(real);
//		final double imAbs=Math.abs(imag);
//		if(reAbs==0.0 && imAbs==0.0)
//			return 0.0;
//		else if(reAbs<imAbs)
//			return imAbs*Math.sqrt(1.0+(real/imag)*(real/imag));
//		else
//			return reAbs*Math.sqrt(1.0+(imag/real)*(imag/real));
//	}

//	public Complex negate() {
//		return createComplex(-real, -imaginary);
//	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(real)
		                            .append(imaginary)
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
		final ComplexStruct rhs = (ComplexStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(real, rhs.real)
		                          .append(imaginary, rhs.imaginary)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(real)
		                                                                .append(imaginary)
		                                                                .toString();
	}
}
