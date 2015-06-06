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
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	/**
	 * {@link ComplexStruct} constant representing I.
	 */
	public static final ComplexStruct I = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);

	/**
	 * {@link ComplexStruct} constant representing -I.
	 */
	public static final ComplexStruct NEGATE_I = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.MINUS_ONE);

	/**
	 * {@link ComplexStruct} constant representing 0.
	 */
	public static final ComplexStruct ZERO = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ZERO);

	/**
	 * {@link ComplexStruct} constant representing 0.0.
	 */
	public static final ComplexStruct ZERO_FLOAT = new ComplexStruct(FloatStruct.ZERO, FloatStruct.ZERO);

	/**
	 * {@link ComplexStruct} constant representing 1.
	 */
	public static final ComplexStruct ONE = new ComplexStruct(IntegerStruct.ONE, IntegerStruct.ZERO);

	/**
	 * {@link ComplexStruct} constant representing 1.0.
	 */
	public static final ComplexStruct ONE_FLOAT = new ComplexStruct(FloatStruct.ONE, FloatStruct.ZERO);

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7848008215064899579L;

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
			coercedReal = real.coerceRealToFloat();
			coercedImaginary = imaginary.coerceRealToFloat();
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
		this.real = real.coerceRealToFloat();
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
		this.imaginary = imaginary.coerceRealToFloat();
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
		this.imaginary = imaginary.coerceRealToFloat();
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
		this.real = real.coerceRealToFloat();
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

	@Override
	public boolean lispEql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean lispEqual(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean lispEqualp(final LispStruct lispStruct) {
		return (lispStruct instanceof NumberStruct) && isEqualTo((NumberStruct) lispStruct);
	}

	@Override
	public RealStruct abs() {
		if (real.zerop()) {
			return imaginary.abs();
		}

		final NumberStruct realSquare = real.multiply(real);
		final NumberStruct imagSquare = imaginary.multiply(imaginary);
		final NumberStruct realSquareImagSquareSum = realSquare.add(imagSquare);

		// Real multiplication and addition will always product another Real so this cast is safe.
		final double sumBigDecimal = ((RealStruct) realSquareImagSquareSum).doubleValue();
		final double sqrtOfSquareSum = FastMath.sqrt(sumBigDecimal);

		if (real instanceof RationalStruct) {
			final BigDecimal bigDecimal = new BigDecimal(sqrtOfSquareSum);

			final boolean isWholeNumber = bigDecimal.setScale(0, RoundingMode.HALF_UP).compareTo(bigDecimal) == 0;
			if (isWholeNumber) {
				return new IntegerStruct(bigDecimal.toBigInteger());
			}
			return new FloatStruct(bigDecimal);
		}
		return new FloatStruct(sqrtOfSquareSum);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this ComplexStruct is zero using {@link #real#zerop} and {@link #imaginary#zerop()}.
	 */
	@Override
	public boolean zerop() {
		return real.zerop() && imaginary.zerop();
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the addition function result for this ComplexStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct add(final NumberStruct number) {
		return ComplexAddStrategy.INSTANCE.add(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the subtraction function result for this ComplexStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct subtract(final NumberStruct number) {
		return ComplexSubtractStrategy.INSTANCE.subtract(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the multiplication function result for this ComplexStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct multiply(final NumberStruct number) {
		return ComplexMultiplyStrategy.INSTANCE.multiply(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the division function result for this ComplexStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct divide(final NumberStruct number) {
		return ComplexDivideStrategy.INSTANCE.divide(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the numeric '=' equality result for this ComplexStruct and the provided {@code number}.
	 */
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
		final NumberStruct imagNegation = imaginary.negation();
		return new ComplexStruct(real, (RealStruct) imagNegation);
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
		final double imagDoubleValue = imaginary.doubleValue();

		final double expReal = FastMath.exp(realDoubleValue);
		final double newReal = expReal * FastMath.cos(imagDoubleValue);
		final double newImag = expReal * FastMath.sin(imagDoubleValue);

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImaginaryFloat = new FloatStruct(newImagBigDecimal);
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
			return ONE_FLOAT;
		}
		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		return ComplexExptStrategy.INSTANCE.expt(this, power);
	}

	@Override
	public NumberStruct log() {
		final RealStruct newReal = abs().log();
		final RealStruct newImag = imaginary.atan(real);
		return new ComplexStruct(newReal, newImag);
	}

	@Override
	public NumberStruct sqrt() {
		if (FloatStruct.ZERO.isEqualTo(real) && FloatStruct.ZERO.isEqualTo(imaginary)) {
			return ZERO;
		}
		final double twoValue = 2.0D;

		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double realAbs = FastMath.abs(realDoubleValue);
		final double thisAbs = abs().doubleValue();
		final double root = FastMath.sqrt((realAbs + thisAbs) / twoValue);

		final double newReal;
		final double newImag;
		if (realDoubleValue >= 0.0D) {
			newReal = root;
			newImag = imagDoubleValue / (twoValue * root);
		} else {
			newReal = FastMath.abs(imagDoubleValue) / (twoValue * root);
			newImag = FastMath.copySign(1.0D, imagDoubleValue) * root;
		}

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct sin() {
		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.sin(realDoubleValue) * FastMath.cosh(imagDoubleValue);
		final double newImag = FastMath.cos(realDoubleValue) * FastMath.sinh(imagDoubleValue);

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct cos() {
		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.cos(realDoubleValue) * FastMath.cosh(imagDoubleValue);
		final double newImag = -FastMath.sin(realDoubleValue) * FastMath.sinh(imagDoubleValue);

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct tan() {
		final double twoValue = 2.0D;

		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double twoAndRealProduct = twoValue * realDoubleValue;
		final double twoAndImagProduct = twoValue * imagDoubleValue;
		final double divisor = FastMath.cos(twoAndRealProduct) + FastMath.cosh(twoAndImagProduct);

		final double newReal = FastMath.sin(twoAndRealProduct) / divisor;
		final double newImag = FastMath.sinh(twoAndImagProduct) / divisor;

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct asin() {
		// asin(z) = -i (log(sqrt(1 - z<sup>2</sup>) + iz))

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
		// acos(z) = -i (log(z + i (sqrt(1 - z<sup>2</sup>))))

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
		// atan(z) = (i / 2) log((i + z) / (i - z))

		final NumberStruct iMinusThis = I.subtract(this);
		final NumberStruct iPlusThis = I.add(this);
		final NumberStruct iDivideByTwo = I.divide(IntegerStruct.TWO);
		return iPlusThis
				.divide(iMinusThis)
				.log()
				.multiply(iDivideByTwo);
	}

	@Override
	public NumberStruct sinh() {
		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.sinh(realDoubleValue) * FastMath.cos(imagDoubleValue);
		final double newImag = FastMath.cosh(realDoubleValue) * FastMath.sin(imagDoubleValue);

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct cosh() {
		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double newReal = FastMath.cosh(realDoubleValue) * FastMath.cos(imagDoubleValue);
		final double newImag = FastMath.sinh(realDoubleValue) * FastMath.sin(imagDoubleValue);

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct tanh() {
		final double twoValue = 2.0D;

		final double realDoubleValue = real.doubleValue();
		final double imagDoubleValue = imaginary.doubleValue();

		final double twoAndRealProduct = twoValue * realDoubleValue;
		final double twoAndImagProduct = twoValue * imagDoubleValue;
		final double divisor = FastMath.cosh(twoAndRealProduct) + FastMath.cos(twoAndImagProduct);

		final double newReal = FastMath.sinh(twoAndRealProduct) / divisor;
		final double newImag = FastMath.sin(twoAndImagProduct) / divisor;

		final BigDecimal newRealBigDecimal = new BigDecimal(newReal);
		final BigDecimal newImagBigDecimal = new BigDecimal(newImag);

		final FloatStruct newRealFloat = new FloatStruct(newRealBigDecimal);
		final FloatStruct newImagFloat = new FloatStruct(newImagBigDecimal);
		return new ComplexStruct(newRealFloat, newImagFloat);
	}

	@Override
	public NumberStruct asinh() {
		if (imaginary.zerop()) {
			return new ComplexStruct(real.asinh(), imaginary);
		}

		// asin(x) = log(x + sqrt(1 + x<sup>2</sup>))
		final NumberStruct thisSquared = multiply(this);
		final NumberStruct onePlusThisSquared = IntegerStruct.ONE.add(thisSquared);
		final NumberStruct squareRootOfPreviousSum = onePlusThisSquared.sqrt();
		final NumberStruct thisPlusSquareRoot = add(squareRootOfPreviousSum);
		return thisPlusSquareRoot.log();
	}

	@Override
	public NumberStruct acosh() {
		if (imaginary.zerop()) {
			return new ComplexStruct(real.acosh(), imaginary);
		}

		// acosh(x) = 2 * (log (sqrt((x + 1) / 2) + sqrt((x - 1) / 2)))
		final NumberStruct thisPlusOne = add(IntegerStruct.ONE);
		final NumberStruct thisPlusOneOverTwo = thisPlusOne.divide(IntegerStruct.TWO);
		final NumberStruct squareRootOfThisPlusOneOverTwo = thisPlusOneOverTwo.sqrt();

		final NumberStruct thisMinusOne = subtract(IntegerStruct.ONE);
		final NumberStruct thisMinusOneOverTwo = thisMinusOne.divide(IntegerStruct.TWO);
		final NumberStruct squareRootOfThisMinusOneOverTwo = thisMinusOneOverTwo.sqrt();

		final NumberStruct sumOfRoots = squareRootOfThisPlusOneOverTwo.add(squareRootOfThisMinusOneOverTwo);
		final NumberStruct logOfSumOfRoots = sumOfRoots.log();

		return IntegerStruct.TWO.multiply(logOfSumOfRoots);
	}

	@Override
	public NumberStruct atanh() {
		if (imaginary.zerop()) {
			return new ComplexStruct(real.atanh(), imaginary);
		}

		// atanh(x) = (log(1 + x) - log(1 - x))/2
		final NumberStruct logOnePlusThis = IntegerStruct.ONE.add(this).log();
		final NumberStruct logOneMinusThis = IntegerStruct.ONE.subtract(this).log();

		final NumberStruct logResultsDifference = logOnePlusThis.subtract(logOneMinusThis);

		return logResultsDifference.divide(IntegerStruct.TWO);
	}

	// Strategy Implementations

	/**
	 * {@link AddStrategy} for computing addition results for {@link ComplexStruct}s.
	 */
	private static class ComplexAddStrategy extends AddStrategy<ComplexStruct> {

		/**
		 * Singleton instance of the {@link ComplexAddStrategy} type.
		 */
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

	/**
	 * {@link SubtractStrategy} for computing subtraction function results for {@link ComplexStruct}s.
	 */
	private static class ComplexSubtractStrategy extends SubtractStrategy<ComplexStruct> {

		/**
		 * Singleton instance of the {@link ComplexSubtractStrategy} type.
		 */
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

	/**
	 * {@link MultiplyStrategy} for computing multiplication function results for {@link ComplexStruct}s.
	 */
	private static class ComplexMultiplyStrategy extends MultiplyStrategy<ComplexStruct> {

		/**
		 * Singleton instance of the {@link ComplexMultiplyStrategy} type.
		 */
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

	/**
	 * {@link DivideStrategy} for computing division function results for {@link ComplexStruct}s.
	 */
	private static class ComplexDivideStrategy extends DivideStrategy<ComplexStruct> {

		/**
		 * Singleton instance of the {@link ComplexDivideStrategy} type.
		 */
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

	/**
	 * {@link EqualToStrategy} for computing numeric '=' equality results for {@link ComplexStruct}s.
	 */
	private static class ComplexEqualToStrategy extends EqualToStrategy<ComplexStruct> {

		/**
		 * Singleton instance of the {@link ComplexEqualToStrategy} type.
		 */
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

		/**
		 * Tests the equality of the provided {@link ComplexStruct} and {@link RealStruct}. A {@link RealStruct} and a
		 * {@link ComplexStruct} are only equal if the {@link ComplexStruct#imaginary} is equivalent to '0' and the
		 * {@link ComplexStruct#real} is equal to the provided {@link RealStruct}.
		 *
		 * @param complex
		 * 		the {@link ComplexStruct} to test for equality to the provided {@link RealStruct}
		 * @param real
		 * 		the {@link RealStruct} to test for equality to the provided {@link ComplexStruct}
		 *
		 * @return true if the number are equivalent; false otherwise
		 */
		private static boolean equalToReal(final ComplexStruct complex, final RealStruct real) {
			final RealStruct realVal1 = complex.getReal();
			final RealStruct imaginaryVal1 = complex.getImaginary();
			return imaginaryVal1.zerop() && realVal1.isEqualTo(real);
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

	/**
	 * {@link ExptStrategy} for computing exponential function results for {@link ComplexStruct}s.
	 */
	private static class ComplexExptStrategy extends ExptStrategy<ComplexStruct> {

		/**
		 * Singleton instance of the {@link ComplexExptStrategy} type.
		 */
		protected static final ComplexExptStrategy INSTANCE = new ComplexExptStrategy();

		@Override
		public NumberStruct expt(final ComplexStruct base, final IntegerStruct power) {
			if (base.getReal() instanceof RationalStruct) {
				return exptInteger(base, power);
			}
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerComplexLogOfBaseProduct = power.multiply(logOfBase);
			return powerComplexLogOfBaseProduct.exp();
		}

		@Override
		public NumberStruct expt(final ComplexStruct base, final FloatStruct power) {
			return exptComplex(base, power);
		}

		@Override
		public NumberStruct expt(final ComplexStruct base, final RatioStruct power) {
			return exptComplex(base, power);
		}

		@Override
		public NumberStruct expt(final ComplexStruct base, final ComplexStruct power) {
			return exptComplex(base, power);
		}

		/**
		 * Determines the exponential result from applying the provided {@link NumberStruct} power to the provided
		 * {@link ComplexStruct} base value.
		 *
		 * @param base
		 * 		the {@link ComplexStruct} to be raised to the provided {@link NumberStruct} power
		 * @param power
		 * 		the {@link NumberStruct} power to raise the provided {@link ComplexStruct} base
		 *
		 * @return exponential result from applying the provided {@link NumberStruct} power to the provided {@link
		 * ComplexStruct} base value
		 */
		protected static NumberStruct exptComplex(final ComplexStruct base, final NumberStruct power) {
			final NumberStruct logOfBase = base.log();
			final NumberStruct powerLogOfBaseProduct = power.multiply(logOfBase);
			return powerLogOfBaseProduct.exp();
		}
	}

	// HashCode / Equals

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
}
