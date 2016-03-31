/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import jcl.classes.BuiltInClassStruct;
import jcl.types.LongFloatType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link BigFloatStruct} is the object representation of a Lisp 'float' type.
 */
public final class BigFloatStruct extends BuiltInClassStruct implements FloatStruct {

	/**
	 * {@link BigFloatStruct} constant representing 0.0.
	 */
	public static final BigFloatStruct ZERO = valueOf(NumberUtils.bigDecimalValue(0.0));

	/**
	 * {@link BigFloatStruct} constant representing -0.0.
	 */
	public static final BigFloatStruct MINUS_ZERO = valueOf(NumberUtils.bigDecimalValue(-0.0));

	/**
	 * {@link BigFloatStruct} constant representing 1.0.
	 */
	public static final BigFloatStruct ONE = valueOf(NumberUtils.bigDecimalValue(1.0));

	/**
	 * {@link BigFloatStruct} constant representing -1.0.
	 */
	public static final BigFloatStruct MINUS_ONE = valueOf(NumberUtils.bigDecimalValue(-1.0));

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(BigFloatStruct.class);

	/**
	 * The floating-point precision of a BigFloatStruct object.
	 */
	private static final int QUADRUPLE_PRECISION = 113;

	/**
	 * The internal {@link BigDecimal} containing the BigFloatStruct contents.
	 */
	final BigDecimal bigDecimal;

	/**
	 * Private constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the BigFloatStruct
	 */
	private BigFloatStruct(final BigDecimal bigDecimal) {
		super(LongFloatType.INSTANCE, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * Returns a BigFloatStruct object with the provided {@link BigDecimal} value.
	 *
	 * @param bigDecimal
	 * 		the {@link BigDecimal} value of the resulting BigFloatStruct
	 *
	 * @return a BigFloatStruct object with the provided {@link BigDecimal} value
	 */
	public static BigFloatStruct valueOf(final BigDecimal bigDecimal) {
		return new BigFloatStruct(bigDecimal);
	}

	/*
		FloatStruct
	 */

	@Override
	public float floatValue() {
		if (LOGGER.isWarnEnabled()) {
			LOGGER.warn("Possible loss of precision.");
		}
		return bigDecimal.floatValue();
	}

	@Override
	public double doubleValue() {
		if (LOGGER.isWarnEnabled()) {
			LOGGER.warn("Possible loss of precision.");
		}
		return bigDecimal.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigDecimal;
	}

	@Override
	public RealStruct.FloatingPointVisitor<?> floatingPointVisitor() {
		return new BigFloatFloatingPointVisitor(this);
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final BigInteger bits = bigDecimal.unscaledValue();
//		final long bits = Double.doubleToRawLongBits(bigDecimal.doubleValue());

		final DecodedQuadruple decodedQuadruple = getDecodedQuadruple(bits);

		final BigInteger mantissa = decodedQuadruple.getMantissa();
		final BigInteger expt = ArithmeticUtils.pow(BigInteger.valueOf(2), QUADRUPLE_PRECISION);
		final BigInteger significand = mantissa.divide(expt);
		final BigFloatStruct significandFloat = new BigFloatStruct(NumberUtils.bigDecimalValue(significand));

		final BigInteger storedExponent = decodedQuadruple.getStoredExponent();
		// 16383 + 112 = 16495
		final BigInteger exponent = storedExponent.subtract(BigInteger.valueOf(16495))
		                                          .add(BigInteger.valueOf(QUADRUPLE_PRECISION));
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponent);

		final int sign = decodedQuadruple.getSign();
		final BigFloatStruct signFloat = (sign == 1) ? ONE : MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final BigInteger bits = bigDecimal.unscaledValue();
		final DecodedQuadruple decodedQuadruple = getDecodedQuadruple(bits);

		final BigInteger mantissa = decodedQuadruple.getMantissa();
		final IntegerStruct significandInteger = IntegerStruct.valueOf(mantissa);

		final BigInteger storedExponent = decodedQuadruple.getStoredExponent();
		// 16383 + 112 = 16495
		final BigInteger exponent = storedExponent.subtract(BigInteger.valueOf(16495));
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponent);

		final int sign = decodedQuadruple.getSign();
		final IntegerStruct signInteger = (sign == 1) ? IntegerStruct.ONE : IntegerStruct.MINUS_ONE;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.valueOf(QUADRUPLE_PRECISION);
	}

	@Override
	public FloatStruct floatSign() {
		// TODO: right?
		final BigInteger bits = bigDecimal.unscaledValue();
		return (bits.compareTo(BigInteger.ZERO) < 0) ? MINUS_ONE : ONE;
	}

	/**
	 * Decodes the bigDecimal by the provided {@link BigInteger} bits into its sign, exponent, and mantissa.
	 *
	 * @param bits
	 * 		the {@link BigInteger} bits representing the {@link BigDecimal} value
	 *
	 * @return the {@link DecodedQuadruple} wrapping the decoded sign, exponent, and mantissa values
	 */
	private static DecodedQuadruple getDecodedQuadruple(final BigInteger bits) {
		final int sign = BigInteger.ZERO.equals(bits.shiftRight(127)) ? 1 : -1;
		final BigInteger exponent = bits.shiftRight(112)
		                                .and(BigInteger.valueOf(0x7fffL));
		final BigInteger mantissa;
		if (BigInteger.ZERO.equals(exponent)) {
			final BigInteger twoToOneTwelveMinusOne =
					BigInteger.valueOf(2)
					          .pow(112)
					          .subtract(BigInteger.ONE);
			mantissa = bits.and(twoToOneTwelveMinusOne)
			               .shiftLeft(1);
		} else {
			final BigInteger twoToOneTwelveMinusOne =
					BigInteger.valueOf(2)
					          .pow(112)
					          .subtract(BigInteger.ONE);
			final BigInteger twoToOneTwelve =
					BigInteger.valueOf(2)
					          .pow(112);
			mantissa = bits.and(twoToOneTwelveMinusOne)
			               .or(twoToOneTwelve);
		}
		return new DecodedQuadruple(mantissa, exponent, sign);
	}

	/**
	 * Decoded wrapper for {@link BigDecimal} sign, exponent, and mantissa values.
	 */
	private static final class DecodedQuadruple {

		/**
		 * The part of the {@link BigDecimal} that represents the significant digits.
		 */
		private final BigInteger mantissa;

		/**
		 * The part of the {@link BigDecimal} that represents the exponent.
		 */
		private final BigInteger storedExponent;

		/**
		 * The part of the {@link BigDecimal} that represents the sign bit.
		 */
		private final int sign;

		/**
		 * Private constructor.
		 *
		 * @param mantissa
		 * 		the part of the {@link BigDecimal} that represents the significant digits
		 * @param storedExponent
		 * 		the part of the {@link BigDecimal} that represents the exponent
		 * @param sign
		 * 		the part of the {@link BigDecimal} that represents the sign bit
		 */
		private DecodedQuadruple(final BigInteger mantissa, final BigInteger storedExponent, final int sign) {
			this.mantissa = mantissa;
			this.storedExponent = storedExponent;
			this.sign = sign;
		}

		/**
		 * Getter for {@link #mantissa} property value.
		 *
		 * @return {@link #mantissa} property value
		 */
		private BigInteger getMantissa() {
			return mantissa;
		}

		/**
		 * Getter for {@link #storedExponent} property value.
		 *
		 * @return {@link #storedExponent} property value
		 */
		private BigInteger getStoredExponent() {
			return storedExponent;
		}

		/**
		 * Getter for {@link #sign} property value.
		 *
		 * @return {@link #sign} property value
		 */
		private int getSign() {
			return sign;
		}
	}

	/*
		RealStruct
	 */

	@Override
	public boolean isLessThan(final LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new BigFloatLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new BigFloatGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new BigFloatLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new BigFloatGreaterThanOrEqualToVisitor(this);
	}

	@Override
	public boolean plusp() {
		return bigDecimal.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigDecimal.signum() < 0;
	}

	@Override
	public RationalStruct rational() {
		final int scale = bigDecimal.scale();

		final BigDecimal movedDecimalPlace = bigDecimal.scaleByPowerOfTen(scale);
		final BigInteger movedDecimalPlaceBigInteger = movedDecimalPlace.toBigInteger();

		final BigFraction bigFraction = new BigFraction(movedDecimalPlaceBigInteger, BigInteger.TEN.pow(scale));
		return RationalStruct.valueOf(bigFraction);
	}

	@Override
	public FloatStruct floatingPoint(final RealStruct.FloatingPointVisitor<?> floatingPointVisitor) {
		return floatingPointVisitor.floatingPoint(this);
	}

	/*
		NumberStruct
	 */

	@Override
	public NumberStruct add(final AddVisitor<?> addVisitor) {
		return addVisitor.add(this);
	}

	@Override
	public AddVisitor<?> addVisitor() {
		return new BigFloatAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new BigFloatSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new BigFloatMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new BigFloatDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new BigFloatEqualToVisitor(this);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			return ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		final ExptVisitor<?> exptVisitor = exptVisitor();
		return power.expt(exptVisitor);
	}

	@Override
	public NumberStruct expt(final ExptVisitor<?> exptVisitor) {
		return exptVisitor.expt(this);
	}

	@Override
	public ExptVisitor<?> exptVisitor() {
		return new BigFloatExptVisitor(this);
	}

	@Override
	public boolean zerop() {
		return bigDecimal.signum() == 0;
	}

	@Override
	public RealStruct abs() {
		if (bigDecimal.signum() >= 0) {
			return this;
		}
		return negation();
	}

	@Override
	public BigFloatStruct negation() {
		if (isEqualTo(ZERO)) {
			return MINUS_ZERO;
		}
		if (isEqualTo(MINUS_ZERO)) {
			return ZERO;
		}
		final BigDecimal negate = bigDecimal.negate();
		return new BigFloatStruct(negate);
	}

	@Override
	public NumberStruct reciprocal() {
		return ONE.divide(this);
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return ONE;
		} else {
			return MINUS_ONE;
		}
	}

	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(bigDecimal)
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
		final BigFloatStruct rhs = (BigFloatStruct) obj;
		return new EqualsBuilder().append(bigDecimal, rhs.bigDecimal)
		                          .isEquals();
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link BigFloatStruct}s.
	 */
	private static final class BigFloatAddVisitor extends RealStruct.RealAddVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatAddVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private BigFloatAddVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.i);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.l);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigInteger);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.f);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.d);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigFraction);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return valueOf(add);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link BigFloatStruct}s.
	 */
	private static final class BigFloatSubtractVisitor extends RealStruct.RealSubtractVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatSubtractVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private BigFloatSubtractVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.i);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.l);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigInteger);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.f);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.d);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigFraction);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return valueOf(subtract);
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * BigFloatStruct}s.
	 */
	private static final class BigFloatMultiplyVisitor extends RealStruct.RealMultiplyVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatMultiplyVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private BigFloatMultiplyVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.i);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.l);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigInteger);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.f);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.d);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigFraction);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return valueOf(multiply);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link BigFloatStruct}s.
	 */
	private static final class BigFloatDivideVisitor extends RealStruct.RealDivideVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatDivideVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private BigFloatDivideVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.i);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.l);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigInteger);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.f);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.d);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = NumberUtils.bigDecimalValue(number2.bigFraction);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return valueOf(divide);
		}
	}

	/**
	 * {@link FloatStruct.FloatEqualToVisitor} for computing numeric '=' equality results for {@link
	 * BigFloatStruct}s.
	 */
	private static final class BigFloatEqualToVisitor extends FloatStruct.FloatEqualToVisitor<BigFloatStruct> {

		/**
		 * Private constructor to make a new instance of an BigFloatEqualToVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private BigFloatEqualToVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) == 0;
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) == 0;
		}

		@Override
		public boolean equalTo(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimal;
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) == 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatLessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * BigFloatStruct}s.
	 */
	private static final class BigFloatLessThanVisitor extends FloatStruct.FloatLessThanVisitor<BigFloatStruct> {

		/**
		 * Private constructor to make a new instance of an BigFloatLessThanVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private BigFloatLessThanVisitor(final BigFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) < 0;
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) < 0;
		}

		@Override
		public boolean lessThan(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) < 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatGreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * BigFloatStruct}s.
	 */
	private static final class BigFloatGreaterThanVisitor extends FloatStruct.FloatGreaterThanVisitor<BigFloatStruct> {

		/**
		 * Private constructor to make a new instance of an BigFloatGreaterThanVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private BigFloatGreaterThanVisitor(final BigFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) > 0;
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) > 0;
		}

		@Override
		public boolean greaterThan(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) > 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatLessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for
	 * {@link BigFloatStruct}s.
	 */
	private static final class BigFloatLessThanOrEqualToVisitor extends FloatStruct.FloatLessThanOrEqualToVisitor<BigFloatStruct> {

		/**
		 * Private constructor to make a new instance of an BigFloatLessThanOrEqualToVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private BigFloatLessThanOrEqualToVisitor(final BigFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) <= 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatGreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for
	 * {@link BigFloatStruct}s.
	 */
	private static final class BigFloatGreaterThanOrEqualToVisitor extends FloatStruct.FloatGreaterThanOrEqualToVisitor<BigFloatStruct> {

		/**
		 * Private constructor to make a new instance of an BigFloatGreaterThanOrEqualToVisitor with the provided
		 * {@link BigFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private BigFloatGreaterThanOrEqualToVisitor(final BigFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimalValue();
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimal;
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link BigFloatStruct}s.
	 */
	private static final class BigFloatExptVisitor extends RealStruct.RealExptVisitor<BigFloatStruct> {
		// TODO: fix

		/**
		 * Private constructor to make a new instance of an BigFloatExptVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private BigFloatExptVisitor(final BigFloatStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.i);
		}

		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.l);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.bigInteger.doubleValue());
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.f);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.d);
		}

		@Override
		public NumberStruct expt(final BigFloatStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.doubleValue());
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			// TODO: more efficient?
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Possible loss of precision.");
			}
			return exptFloatRatioNew(base.bigDecimal.doubleValue(), power.bigFraction.doubleValue());
		}
	}

	/**
	 * {@link RealStruct.FloatingPointVisitor} for converting a {@link RealStruct} to the {@link BigFloatStruct}
	 * type of {@link #prototype}.
	 */
	private static final class BigFloatFloatingPointVisitor extends RealStruct.FloatingPointVisitor<BigFloatStruct> {

		/**
		 * Private constructor to make a new instance of an BigFloatFloatingPointVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param prototype
		 * 		the prototype argument in the floating-point conversion operation
		 */
		private BigFloatFloatingPointVisitor(final BigFloatStruct prototype) {
			super(prototype);
		}

		@Override
		public FloatStruct floatingPoint(final IntIntegerStruct real) {
			return valueOf(NumberUtils.bigDecimalValue(real.i));
		}

		@Override
		public FloatStruct floatingPoint(final LongIntegerStruct real) {
			return valueOf(NumberUtils.bigDecimalValue(real.l));
		}

		@Override
		public FloatStruct floatingPoint(final BigIntegerStruct real) {
			return valueOf(NumberUtils.bigDecimalValue(real.bigInteger));
		}

		@Override
		public FloatStruct floatingPoint(final SingleFloatStruct real) {
			return valueOf(NumberUtils.bigDecimalValue(real.f));
		}

		@Override
		public FloatStruct floatingPoint(final DoubleFloatStruct real) {
			return valueOf(NumberUtils.bigDecimalValue(real.d));
		}

		@Override
		public FloatStruct floatingPoint(final BigFloatStruct real) {
			return real;
		}

		@Override
		public FloatStruct floatingPoint(final RatioStruct real) {
			return valueOf(NumberUtils.bigDecimalValue(real.bigFraction));
		}
	}
}
