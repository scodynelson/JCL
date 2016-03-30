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
	 * The floating-point precision of a SingleFloatStruct object.
	 */
	private static final int FLOAT_PRECISION = 113;

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
		// TODO: Warn loss of precision
		return bigDecimal.floatValue();
	}

	@Override
	public double doubleValue() {
		// TODO: Warn loss of precision
		return bigDecimal.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigDecimal;
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final int decodedExponentDiffer = 1075;
		final int doubleFloatingPointPrecision = 53;

		final long bits = Double.doubleToRawLongBits(apfloatValue().doubleValue());
		final DecodedDoubleRaw decodedDoubleRaw = getDecodedQuadrupleRaw(BigInteger.valueOf(bits));

		final long mantissa = decodedDoubleRaw.getMantissa();
		final BigDecimal mantissaBigDecimal = NumberUtils.bigDecimalValue(mantissa);

		final double expt = StrictMath.pow(2, doubleFloatingPointPrecision);
		final BigDecimal exptBigDecimal = NumberUtils.bigDecimalValue(expt);

		final BigDecimal significand = mantissaBigDecimal.divide(exptBigDecimal, MathContext.DECIMAL128);
		final BigFloatStruct significandFloat = new BigFloatStruct(significand);

		final long storedExponent = decodedDoubleRaw.getStoredExponent();
		final long exponent = (storedExponent - decodedExponentDiffer) + doubleFloatingPointPrecision;
		final BigInteger exponentBigInteger = BigInteger.valueOf(exponent);
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponentBigInteger);

		final long sign = decodedDoubleRaw.getSign();
		final BigDecimal signBigDecimal = NumberUtils.bigDecimalValue(sign);
		final BigFloatStruct signFloat = new BigFloatStruct(signBigDecimal);

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public NumberStruct scaleFloat(final IntegerStruct scale) {
		final IntegerStruct radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	@Override
	public IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	@Override
	public FloatStruct floatSign() {
		return floatSign(ONE);
	}

	@Override
	public FloatStruct floatSign(final FloatStruct float2) {
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				final BigDecimal subtract = BigDecimal.ZERO.subtract(float2.getBigDecimal());
				return new BigFloatStruct(subtract);
			}
		} else {
			return (BigFloatStruct) float2.abs();
		}
	}

	@Override
	public IntegerStruct floatDigits() {
		return floatPrecision();
	}

	/*
	 * http://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format
	 */
	@Override
	public IntegerStruct floatPrecision() {
//		final int binary32Precision = 24;
//		final int binary64Precision = 53;
		final int binary128Precision = 113;
		return IntegerStruct.valueOf(BigInteger.valueOf(binary128Precision));
	}

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * calculation for these values are based on the decoding for Java {@link Double} values from the algorithm defined
	 * in {@link Double#longBitsToDouble}. The difference between this method an {@link #decodeFloat()} is that the
	 * significand and sign will both be {@link IntegerStruct}s with a special weighting between the significand and
	 * exponent based on the scaling needed for the significand to produce an {@link IntegerStruct}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final int decodedExponentDiffer = 1075;

		final long bits = Double.doubleToRawLongBits(doubleValue());
		final DecodedDoubleRaw decodedDoubleRaw = getDecodedQuadrupleRaw(BigInteger.valueOf(bits));

		final long mantissa = decodedDoubleRaw.getMantissa();
		final BigInteger mantissaBigInteger = BigInteger.valueOf(mantissa);
		final IntegerStruct significandInteger = IntegerStruct.valueOf(mantissaBigInteger);

		final long storedExponent = decodedDoubleRaw.getStoredExponent();
		final long exponent = storedExponent - decodedExponentDiffer;
		final BigInteger exponentBigInteger = BigInteger.valueOf(exponent);
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponentBigInteger);

		final long sign = decodedDoubleRaw.getSign();
		final BigInteger signBigInteger = BigInteger.valueOf(sign);
		final IntegerStruct signInteger = IntegerStruct.valueOf(signBigInteger);

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	/*
	 * See {@link https://docs.oracle.com/javase/8/docs/api/java/lang/Quadruple.html} for details.
	 * <p>
	 * The following is per the JVM spec section 4.4.5
	 */
	@SuppressWarnings("all")
	private static DecodedDoubleRaw getDecodedQuadrupleRaw(final BigInteger bits) {
		final BigInteger sign = BigInteger.ZERO.equals(bits.shiftRight(127)) ? BigInteger.ONE : BigInteger.valueOf(-1);
//		 16382 == exponent max
		final BigInteger exponent = bits.shiftRight(112).and(BigInteger.valueOf(16382));
		final BigInteger mantissa;
		if (BigInteger.ZERO.equals(exponent)) {
			BigInteger twoToOneTwelveMinusOne = new BigInteger("5192296858534827628530496329220095");
			mantissa = (bits.and(twoToOneTwelveMinusOne)).shiftLeft(1);
		} else {
			BigInteger twoToOneTwelveMinusOne = new BigInteger("5192296858534827628530496329220095");
			BigInteger twoToOneTwelve = new BigInteger("5192296858534827628530496329220096");
			mantissa = (bits.and(twoToOneTwelveMinusOne)).or(twoToOneTwelve);
		}
		return null;
//		return new DecodedDoubleRaw(mantissa, exponent, sign);
	}

	/*
		RealStruct
	 */

	@Override
	public boolean isLessThan(final LessThanVisitor<?> lessThanVisitor) {
		return lessThanVisitor.lessThan(this);
	}

	@Override
	public boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
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
		final BigFraction bigFractionReduced = bigFraction.reduce();
		return RationalStruct.valueOf(bigFractionReduced);
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
		return new FloatAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new FloatSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new FloatMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new FloatDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		// TODO: customized visitor???
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
	private static final class FloatAddVisitor extends RealStruct.RealAddVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatAddVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private FloatAddVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			return addFloat(number1, number2);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			return addFloat(number1, number2);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			return addFloat(number1, number2);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			return super.add(number2);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			return super.add(number2);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			return super.add(number2);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			return addFloat(number1, number2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link BigFloatStruct}s.
	 */
	private static final class FloatSubtractVisitor extends RealStruct.RealSubtractVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatSubtractVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private FloatSubtractVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			return subtractFloat(number1, number2);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			return subtractFloat(number1, number2);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			return subtractFloat(number1, number2);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			return super.subtract(number2);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			return super.subtract(number2);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			return super.subtract(number2);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			return subtractFloat(number1, number2);
		}

		private static RealStruct subtractFloat(final BigFloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new BigFloatStruct(subtract);
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * BigFloatStruct}s.
	 */
	private static final class FloatMultiplyVisitor extends RealStruct.RealMultiplyVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatMultiplyVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private FloatMultiplyVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			return multiplyFloat(number1, number2);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			return multiplyFloat(number1, number2);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			return multiplyFloat(number1, number2);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			return super.multiply(number2);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			return super.multiply(number2);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			return super.multiply(number2);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			return multiplyFloat(number1, number2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link BigFloatStruct}s.
	 */
	private static final class FloatDivideVisitor extends RealStruct.RealDivideVisitor<BigFloatStruct> {

		/**
		 * Package private constructor to make a new instance of an FloatDivideVisitor with the provided {@link
		 * BigFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private FloatDivideVisitor(final BigFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			return divideFloat(number1, number2);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			return divideFloat(number1, number2);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			return divideFloat(number1, number2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			return super.divide(number2);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			return super.divide(number2);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			return super.divide(number2);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			return divideFloat(number1, number2);
		}

		private static RealStruct divideFloat(final BigFloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new BigFloatStruct(divide);
		}
	}
}
