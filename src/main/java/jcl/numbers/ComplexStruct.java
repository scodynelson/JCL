/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import jcl.LispStruct;
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

	public static final ComplexStruct ONE_FLOAT = new ComplexStruct(FloatStruct.ONE, FloatStruct.ZERO);

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

		RealStruct coercedReal = real;
		RealStruct coercedImaginary = imaginary;
		if ((real instanceof FloatStruct) || (imaginary instanceof FloatStruct)) {
			coercedReal = coerceRealToFloat(real);
			coercedImaginary = coerceRealToFloat(coercedImaginary);
		}
		this.real = coercedReal;
		this.imaginary = coercedImaginary;
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
		this.real = coerceRealToFloat(real);
		this.imaginary = imaginary;
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
		this.imaginary = coerceRealToFloat(imaginary);
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
		this.imaginary = coerceRealToFloat(imaginary);
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
		this.real = coerceRealToFloat(real);
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

	private static FloatStruct coerceRealToFloat(final RealStruct realVal) {
		if (realVal instanceof FloatStruct) {
			return (FloatStruct) realVal;
		}
		final BigDecimal bigDecimal = realVal.bigDecimalValue();
		return new FloatStruct(bigDecimal);
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
		return (lispStruct instanceof NumberStruct) && isEqualTo((NumberStruct) lispStruct);
	}

	@Override
	public RealStruct abs() {
		if (real.zerop()) {
			return imaginary.abs();
		}

		final NumberStruct realSquare = real.multiply(real);
		final NumberStruct imaginarySquare = imaginary.multiply(imaginary);
		final NumberStruct realSquareImaginarySquareSum = realSquare.add(imaginarySquare);

		// Real multiplication and addition will always product another Real so this cast is safe.
		final double sumBigDecimal  = ((RealStruct) realSquareImaginarySquareSum).doubleValue();
		final double sqrtOfSquareSum = FastMath.sqrt(sumBigDecimal);

		if (real instanceof RationalStruct) {
			final BigDecimal bigDecimal = new BigDecimal(sqrtOfSquareSum);
			if (isWholeNumber(bigDecimal)) {
				return new IntegerStruct(bigDecimal.toBigInteger());
			}
			return new FloatStruct(bigDecimal);
		}
		return new FloatStruct(sqrtOfSquareSum);
	}

	private static boolean isWholeNumber(final BigDecimal bigDecimal) {
		return bigDecimal.setScale(0, RoundingMode.HALF_UP).compareTo(bigDecimal) == 0;
	}

	@Override
	public boolean zerop() {
		return real.zerop() && imaginary.zerop();
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		return ComplexAddStrategy.INSTANCE.add(this, number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		return ComplexSubtractStrategy.INSTANCE.subtract(this, number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		return ComplexMultiplyStrategy.INSTANCE.multiply(this, number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		return ComplexDivideStrategy.INSTANCE.divide(this, number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		return ComplexEqualToStrategy.INSTANCE.equalTo(this, number);
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
	public RealStruct realPart() {
		return real;
	}

	@Override
	public RealStruct imagPart() {
		return imaginary;
	}

	@Override
	public NumberStruct conjugate() {
		final NumberStruct negateImag = imaginary.negation();
		return new ComplexStruct(real, (RealStruct) negateImag);
	}

	@Override
	public NumberStruct negation() {
		return new ComplexStruct((RealStruct) real.negation(), (RealStruct) imaginary.negation());
	}

	@Override
	public NumberStruct reciprocal() {
		return ONE.divide(this);
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
					return ONE_FLOAT;
				}
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}
		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		return ComplexExptStrategy.INSTANCE.expt(this, power);
	}

	@Override
	public NumberStruct log() {
		final RealStruct newReal = abs().log();
		final RealStruct newImaginary = imaginary.atan(real);
		return new ComplexStruct(newReal, newImaginary);
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

	// Strategy Implementations

	private static class ComplexAddStrategy extends AddStrategy<ComplexStruct> {

		private static final ComplexAddStrategy INSTANCE = new ComplexAddStrategy();

		@Override
		public NumberStruct add(final ComplexStruct number1, final IntegerStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct add = realVal.add(number2);
			return new ComplexStruct((RealStruct) add, imaginaryVal);
		}

		@Override
		public NumberStruct add(final ComplexStruct number1, final FloatStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct add = realVal.add(number2);
			return new ComplexStruct((RealStruct) add, imaginaryVal);
		}

		@Override
		public NumberStruct add(final ComplexStruct number1, final RatioStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct add = realVal.add(number2);
			return new ComplexStruct((RealStruct) add, imaginaryVal);
		}

		@Override
		public NumberStruct add(final ComplexStruct number1, final ComplexStruct number2) {
			final RealStruct realVal1 = number1.getReal();
			final RealStruct imaginaryVal1 = number1.getImaginary();

			final RealStruct realVal2 = number2.getReal();
			final RealStruct imaginaryVal2 = number2.getImaginary();

			final NumberStruct addReal = realVal1.add(realVal2);
			final NumberStruct addImag = imaginaryVal1.add(imaginaryVal2);

			if (addImag.zerop()) {
				return addReal;
			}

			return new ComplexStruct((RealStruct) addReal, (RealStruct) addImag);
		}
	}

	private static class ComplexSubtractStrategy extends SubtractStrategy<ComplexStruct> {

		private static final ComplexSubtractStrategy INSTANCE = new ComplexSubtractStrategy();

		@Override
		public NumberStruct subtract(final ComplexStruct number1, final IntegerStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct subtract = realVal.subtract(number2);
			return new ComplexStruct((RealStruct) subtract, imaginaryVal);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number1, final FloatStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct subtract = realVal.subtract(number2);
			return new ComplexStruct((RealStruct) subtract, imaginaryVal);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number1, final RatioStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct subtract = realVal.subtract(number2);
			return new ComplexStruct((RealStruct) subtract, imaginaryVal);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number1, final ComplexStruct number2) {
			final RealStruct realVal1 = number1.getReal();
			final RealStruct imaginaryVal1 = number1.getImaginary();

			final RealStruct realVal2 = number2.getReal();
			final RealStruct imaginaryVal2 = number2.getImaginary();

			final NumberStruct subtractReal = realVal1.subtract(realVal2);
			final NumberStruct subtractImag = imaginaryVal1.subtract(imaginaryVal2);

			if (subtractImag.zerop()) {
				return subtractReal;
			}

			return new ComplexStruct((RealStruct) subtractReal, (RealStruct) subtractImag);
		}
	}

	private static class ComplexMultiplyStrategy extends MultiplyStrategy<ComplexStruct> {

		private static final ComplexMultiplyStrategy INSTANCE = new ComplexMultiplyStrategy();

		@Override
		public NumberStruct multiply(final ComplexStruct number1, final IntegerStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct multiplyReal = realVal.multiply(number2);
			final NumberStruct multiplyImag = imaginaryVal.multiply(number2);
			return new ComplexStruct((RealStruct) multiplyReal, (RealStruct) multiplyImag);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number1, final FloatStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct multiplyReal = realVal.multiply(number2);
			final NumberStruct multiplyImag = imaginaryVal.multiply(number2);
			return new ComplexStruct((RealStruct) multiplyReal, (RealStruct) multiplyImag);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number1, final RatioStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct multiplyReal = realVal.multiply(number2);
			final NumberStruct multiplyImag = imaginaryVal.multiply(number2);
			return new ComplexStruct((RealStruct) multiplyReal, (RealStruct) multiplyImag);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number1, final ComplexStruct number2) {
			final NumberStruct realVal1 = number1.getReal();
			final NumberStruct imaginaryVal1 = number1.getImaginary();

			final NumberStruct realVal2 = number2.getReal();
			final NumberStruct imaginaryVal2 = number2.getImaginary();

			// Algorithm:
			// real part = ac - bd
			// imag part = i(ad + bc)
			final NumberStruct ac = realVal1.multiply(realVal2);
			final NumberStruct bd = imaginaryVal1.multiply(imaginaryVal2);
			final NumberStruct ad = realVal1.multiply(imaginaryVal2);
			final NumberStruct bc = imaginaryVal1.multiply(realVal2);

			final NumberStruct acSubtractBd = ac.subtract(bd);
			final NumberStruct adAddBc = ad.add(bc);

			if (adAddBc.zerop()) {
				return acSubtractBd;
			}

			return new ComplexStruct((RealStruct) acSubtractBd, (RealStruct) adAddBc);
		}
	}

	private static class ComplexDivideStrategy extends DivideStrategy<ComplexStruct> {

		private static final ComplexDivideStrategy INSTANCE = new ComplexDivideStrategy();

		@Override
		public NumberStruct divide(final ComplexStruct number1, final IntegerStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct divideReal = realVal.divide(number2);
			final NumberStruct divideImag = imaginaryVal.divide(number2);
			return new ComplexStruct((RealStruct) divideReal, (RealStruct) divideImag);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number1, final FloatStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct divideReal = realVal.divide(number2);
			final NumberStruct divideImag = imaginaryVal.divide(number2);
			return new ComplexStruct((RealStruct) divideReal, (RealStruct) divideImag);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number1, final RatioStruct number2) {
			final RealStruct realVal = number1.getReal();
			final RealStruct imaginaryVal = number1.getImaginary();

			final NumberStruct divideReal = realVal.divide(number2);
			final NumberStruct divideImag = imaginaryVal.divide(number2);
			return new ComplexStruct((RealStruct) divideReal, (RealStruct) divideImag);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number1, final ComplexStruct number2) {
			final NumberStruct realVal1 = number1.getReal();
			final NumberStruct imaginaryVal1 = number1.getImaginary();

			final NumberStruct realVal2 = number2.getReal();
			final NumberStruct imaginaryVal2 = number2.getImaginary();

			// Algorithm:
			// sum = (cc + dd)
			// real part = (ac + bd) / sum
			// imag part = i(bc - ad) / sum
			final NumberStruct ac = realVal1.multiply(realVal2);
			final NumberStruct bd = imaginaryVal1.multiply(imaginaryVal2);
			final NumberStruct bc = imaginaryVal1.multiply(realVal2);
			final NumberStruct ad = realVal1.multiply(imaginaryVal2);

			final NumberStruct squareRealVal2 = realVal2.multiply(realVal2);
			final NumberStruct squareImagVal2 = imaginaryVal2.multiply(imaginaryVal2);
			final NumberStruct squareRealSquareImagSum = squareRealVal2.add(squareImagVal2);

			final NumberStruct acAddBd = ac.add(bd);
			final NumberStruct bcSubtractAd = bc.subtract(ad);

			final NumberStruct acAddBdDivideBySumOfSquares = acAddBd.divide(squareRealSquareImagSum);
			final NumberStruct bcSubtractAdDivideBySumOfSquares = bcSubtractAd.divide(squareRealSquareImagSum);

			if (bcSubtractAdDivideBySumOfSquares.zerop()) {
				return acAddBdDivideBySumOfSquares;
			}

			return new ComplexStruct((RealStruct) acAddBdDivideBySumOfSquares, (RealStruct) bcSubtractAdDivideBySumOfSquares);
		}
	}

	private static class ComplexEqualToStrategy extends EqualToStrategy<ComplexStruct> {

		private static final ComplexEqualToStrategy INSTANCE = new ComplexEqualToStrategy();

		@Override
		public boolean equalTo(final ComplexStruct number1, final IntegerStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final ComplexStruct number1, final FloatStruct number2) {
			return equalToReal(number1, number2);
		}

		@Override
		public boolean equalTo(final ComplexStruct number1, final RatioStruct number2) {
			return equalToReal(number1, number2);
		}

		private static boolean equalToReal(final ComplexStruct number1, final RealStruct number2) {
			final RealStruct realVal1 = number1.getReal();
			final RealStruct imaginaryVal1 = number1.getImaginary();

			if (imaginaryVal1 instanceof FloatStruct) {
				final FloatStruct realFloat1 = (FloatStruct) realVal1;
				final FloatStruct imaginaryFloat1 = (FloatStruct) imaginaryVal1;

				final BigDecimal realBigDecimal1 = realFloat1.getBigDecimal();
				final BigDecimal imagBigDecimal1 = imaginaryFloat1.getBigDecimal();
				if (imagBigDecimal1.compareTo(BigDecimal.ZERO) == 0) {

					final BigDecimal bigDecimal2 = number2.bigDecimalValue();
					return bigDecimal2.compareTo(realBigDecimal1) == 0;
				}
			}

			return false;
		}

		@Override
		public boolean equalTo(final ComplexStruct number1, final ComplexStruct number2) {
			final RealStruct realVal1 = number1.getReal();
			final RealStruct imaginaryVal1 = number1.getImaginary();

			final RealStruct realVal2 = number2.getReal();
			final RealStruct imaginaryVal2 = number2.getImaginary();

			return realVal1.isEqualTo(realVal2) && imaginaryVal1.isEqualTo(imaginaryVal2);
		}
	}

	private static class ComplexExptStrategy extends ExptStrategy<ComplexStruct> {

		protected static final ComplexExptStrategy INSTANCE = new ComplexExptStrategy();

		@Override
		public NumberStruct expt(final ComplexStruct number1, final IntegerStruct number2) {
			if (number1.getReal() instanceof RationalStruct) {
				return exptInteger(number1, number2);
			}
			final NumberStruct logOfBase = number1.log();
			final NumberStruct powerComplexLogOfBaseProduct = number2.multiply(logOfBase);
			return powerComplexLogOfBaseProduct.exp();
		}

		@Override
		public NumberStruct expt(final ComplexStruct number1, final FloatStruct number2) {
			return exptComplex(number1, number2);
		}

		@Override
		public NumberStruct expt(final ComplexStruct number1, final RatioStruct number2) {
			return exptComplex(number1, number2);
		}

		@Override
		public NumberStruct expt(final ComplexStruct number1, final ComplexStruct number2) {
			return exptComplex(number1, number2);
		}

		protected static NumberStruct exptComplex(final ComplexStruct base, final NumberStruct power) {
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerLogOfBaseProduct = power.multiply(logOfBase);
			return powerLogOfBaseProduct.exp();
		}
	}

	// HashCode / Equals / ToString

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
