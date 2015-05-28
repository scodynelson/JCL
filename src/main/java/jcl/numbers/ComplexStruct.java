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

	public static final ComplexStruct NEGATE_I = new ComplexStruct(FloatStruct.MINUS_ZERO, FloatStruct.MINUS_ONE);

	public static final ComplexStruct ZERO = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ZERO);

	public static final ComplexStruct ONE = new ComplexStruct(IntegerStruct.ONE, IntegerStruct.ZERO);

	public static final ComplexStruct TWO = new ComplexStruct(IntegerStruct.TWO, IntegerStruct.ZERO);

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
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double expReal = FastMath.exp(realDoubleValue);
		final double newReal = expReal * FastMath.cos(imaginaryDoubleValue);
		final double newImaginary = expReal * FastMath.sin(imaginaryDoubleValue);

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
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
				|| isComplexFloat(this)
				|| ((power instanceof ComplexStruct)
				&& isComplexFloat((ComplexStruct) power))) {
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

	private boolean isComplexFloat(final ComplexStruct power) {
		return (power.real instanceof FloatStruct) || (power.imaginary instanceof FloatStruct);
	}

	@Override
	public NumberStruct sqrt() {
		if (FloatStruct.ZERO.equals(real) && FloatStruct.ZERO.equals(imaginary)) {
			return ZERO;
		}

		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double realAbs = FastMath.abs(realDoubleValue);
		final double thisAbs = abs().doubleValue();
		final double root = FastMath.sqrt((realAbs + thisAbs) / 2.0D);
		if (realDoubleValue >= 0.0) {
			final double newImaginary = imaginaryDoubleValue / (2.0D * root);

			final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(root));
			final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
			return new ComplexStruct(newRealFloat, newImaginaryFloat);
		} else {
			final double newReal = FastMath.abs(imaginaryDoubleValue) / (2.0D * root);
			final double newImaginary = FastMath.copySign(1.0D, imaginaryDoubleValue) * root;

			final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
			final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
			return new ComplexStruct(newRealFloat, newImaginaryFloat);
		}
	}

	@Override
	public NumberStruct log() {
		// TODO: check complex results when item is not complex!!!
		// TODO: no casting!!!
		final RealStruct newReal = abs().log();
		final RealStruct newImaginary = imaginary.atan(real);

		return new ComplexStruct(newReal, newImaginary);
	}

	@Override
	public NumberStruct sin() {
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.sin(realDoubleValue) * FastMath.cosh(imaginaryDoubleValue);
		final double newImaginary = FastMath.cos(realDoubleValue) * FastMath.sinh(imaginaryDoubleValue);

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
	}

	@Override
	public NumberStruct cos() {
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.cos(realDoubleValue) * FastMath.cosh(imaginaryDoubleValue);
		final double newImaginary = -FastMath.sin(realDoubleValue) * FastMath.sinh(imaginaryDoubleValue);

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
	}

	@Override
	public NumberStruct tan() {
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double real2 = 2.0D * realDoubleValue;
		final double imaginary2 = 2.0D * imaginaryDoubleValue;
		final double divisor = FastMath.cos(real2) + FastMath.cosh(imaginary2);

		final double newReal = FastMath.sin(real2) / divisor;
		final double newImaginary = FastMath.sinh(imaginary2) / divisor;

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
	}

	@Override
	public NumberStruct asin() {
		final NumberStruct thisSquared = multiply(this);
		final NumberStruct thisMultiplyByI = multiply(I);
		return ONE.subtract(thisSquared)
		          .sqrt()
		          .add(thisMultiplyByI)
		          .log()
		          .multiply(NEGATE_I);
	}

	@Override
	public NumberStruct acos() {
		final NumberStruct thisSquared = multiply(this);
		final NumberStruct pieceToAdd = ONE.subtract(thisSquared)
		                                   .sqrt()
		                                   .multiply(I);
		return add(pieceToAdd)
				.log()
				.multiply(NEGATE_I);
	}

	@Override
	public NumberStruct atan() {
		final NumberStruct iMinusThis = I.subtract(this);
		final NumberStruct iDivideByTwo = I.divide(TWO);
		return add(I)
				.divide(iMinusThis)
				.log()
				.multiply(iDivideByTwo);
	}

	@Override
	public NumberStruct sinh() {
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.sinh(realDoubleValue) * FastMath.cos(imaginaryDoubleValue);
		final double newImaginary = FastMath.cosh(realDoubleValue) * FastMath.sin(imaginaryDoubleValue);

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
	}

	@Override
	public NumberStruct cosh() {
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.cosh(realDoubleValue) * FastMath.cos(imaginaryDoubleValue);
		final double newImaginary = FastMath.sinh(realDoubleValue) * FastMath.sin(imaginaryDoubleValue);

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
	}

	@Override
	public NumberStruct tanh() {
		final double realDoubleValue = real.doubleValue();
		final double imaginaryDoubleValue = imaginary.doubleValue();

		final double real2 = 2.0D * realDoubleValue;
		final double imaginary2 = 2.0D * imaginaryDoubleValue;
		final double divisor = FastMath.cosh(real2) + FastMath.cos(imaginary2);

		final double newReal = FastMath.sinh(real2) / divisor;
		final double newImaginary = FastMath.sin(imaginary2) / divisor;

		final FloatStruct newRealFloat = new FloatStruct(BigDecimal.valueOf(newReal));
		final FloatStruct newImaginaryFloat = new FloatStruct(BigDecimal.valueOf(newImaginary));
		return new ComplexStruct(newRealFloat, newImaginaryFloat);
	}

	@Override
	public NumberStruct asinh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct(real.asinh(), im);
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
			return new ComplexStruct(real.acosh(), im);
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
			return new ComplexStruct(real.atanh(), im);
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

	@Override
	public RealStruct realPart() {
		return real;
	}

	@Override
	public RealStruct imagPart() {
		return imaginary;
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
	public NumberStruct conjugate() {
		return new ComplexStruct(real, (RealStruct) imaginary.negate());
	}

	@Override
	public NumberStruct negate() {
		return new ComplexStruct((RealStruct) real.negate(), imaginary);
	}

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
