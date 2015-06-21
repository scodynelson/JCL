/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.LispStruct;
import jcl.types.FloatType;
import jcl.types.SingleFloatType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;
import org.apfloat.Apfloat;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	/**
	 * {@link FloatStruct} constant representing 0.0.
	 */
	public static final FloatStruct ZERO = new FloatStruct(BigDecimal.valueOf(0.0));

	/**
	 * {@link FloatStruct} constant representing -0.0.
	 */
	public static final FloatStruct MINUS_ZERO = new FloatStruct(BigDecimal.valueOf(-0.0));

	/**
	 * {@link FloatStruct} constant representing 1.0.
	 */
	public static final FloatStruct ONE = new FloatStruct(BigDecimal.valueOf(1.0));

	/**
	 * {@link FloatStruct} constant representing -1.0.
	 */
	public static final FloatStruct MINUS_ONE = new FloatStruct(BigDecimal.valueOf(-1.0));

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4803312076840516559L;

	/**
	 * The internal {@link BigDecimal} containing the float contents.
	 */
	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final Apfloat apfloat) {
		this(apfloat.doubleValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param doubleValue
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final double doubleValue) {
		this(BigDecimal.valueOf(doubleValue));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloatType.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatType
	 * 		a {@link FloatType} that represents the type of {@link FloatType}
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final FloatType floatType, final BigDecimal bigDecimal) {
		super(floatType, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * Getter for float {@link #bigDecimal} property.
	 *
	 * @return float {@link #bigDecimal} property
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the absolute value of this FloatStruct.
	 */
	@Override
	public RealStruct abs() {
		if (bigDecimal.signum() >= 0) {
			return this;
		}
		final BigDecimal negate = bigDecimal.negate();
		return new FloatStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this FloatStruct is zero using {@link #bigDecimal#signum}.
	 */
	@Override
	public boolean zerop() {
		return bigDecimal.signum() == 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this FloatStruct is positive using {@link #bigDecimal#signum}.
	 */
	@Override
	public boolean plusp() {
		return bigDecimal.signum() > 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines whether or not this FloatStruct is negative using {@link #bigDecimal#signum}.
	 */
	@Override
	public boolean minusp() {
		return bigDecimal.signum() < 0;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the addition function result for this FloatStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct add(final NumberStruct number) {
		return FloatAddStrategy.INSTANCE.add(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the subtraction function result for this FloatStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct subtract(final NumberStruct number) {
		return FloatSubtractStrategy.INSTANCE.subtract(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the multiplication function result for this FloatStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct multiply(final NumberStruct number) {
		return FloatMultiplyStrategy.INSTANCE.multiply(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the division function result for this FloatStruct and the provided {@code number}.
	 */
	@Override
	public NumberStruct divide(final NumberStruct number) {
		return FloatDivideStrategy.INSTANCE.divide(this, number);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Determines the whether or not the numerical value of this FloatStruct is zero, positive, or negative,
	 * returning {@code this}, {@link #ONE}, or {@link #MINUS_ONE} respectively.
	 */
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

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ZERO} as the imaginary part of FloatStruct is always '0'.
	 */
	@Override
	public RealStruct imagPart() {
		return ZERO;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the negation with {@link #bigDecimal#negation} and the creating a new FloatStruct to wrap it. If this
	 * FloatStruct is numerically equivalent to {@link #ZERO}, {@link #MINUS_ZERO} is returned. If this FloatStruct is
	 * numerically equivalent to {@link #MINUS_ZERO}, {@link #ZERO} is returned.
	 */
	@Override
	public NumberStruct negation() {
		if (isEqualTo(ZERO)) {
			return MINUS_ZERO;
		}
		if (isEqualTo(MINUS_ZERO)) {
			return ZERO;
		}
		final BigDecimal negate = bigDecimal.negate();
		return new FloatStruct(negate);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the reciprocal of this FloatStruct by dividing {@link #ONE} by this FloatStruct.
	 */
	@Override
	public NumberStruct reciprocal() {
		return ONE.divide(this);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the exponential function result for this FloatStruct as this {@code base} and the provided {@link
	 * NumberStruct} as the {@code power}. If {@code power} is '0', {@link #ONE} is returned. If this FloatStruct is
	 * either '0' or '1', {@code this} is returned.
	 */
	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			return ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		return RealExptStrategy.INSTANCE.expt(this, power);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #bigDecimal}.
	 */
	@Override
	public BigDecimal bigDecimalValue() {
		return bigDecimal;
	}

	@Override
	public Apfloat apfloatValue() {
		return new Apfloat(bigDecimal);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link this} as it is already a FloatStruct.
	 */
	@Override
	public FloatStruct coerceRealToFloat() {
		return this;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Returns {@link #ZERO} as a '0' FloatStruct value.
	 */
	@Override
	public RealStruct zeroValue() {
		return ZERO;
	}

	@Override
	public RationalStruct rational() {
		final int scale = bigDecimal.scale();

		final BigDecimal movedDecimalPlace = bigDecimal.scaleByPowerOfTen(scale);
		final BigInteger movedDecimalPlaceBigInteger = movedDecimalPlace.toBigInteger();

		final BigFraction bigFraction = new BigFraction(movedDecimalPlaceBigInteger, BigInteger.TEN.pow(scale));
		final BigFraction bigFractionReduced = bigFraction.reduce();

		final BigInteger denominator = bigFractionReduced.getDenominator();
		if (BigInteger.ONE.compareTo(denominator) == 0) {
			final BigInteger numerator = bigFractionReduced.getNumerator();
			return new IntegerStruct(numerator);
		}

		return new RatioStruct(bigFractionReduced);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the floor of the result of this FloatStruct with the provided {@code divisor}.
	 */
	@Override
	public QuotientRemainderResult floor(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.floor(this, divisor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the floor of the result of this FloatStruct with the provided {@code divisor}, returning the quotient
	 * as a FloatStruct.
	 */
	@Override
	public QuotientRemainderResult ffloor(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.ffloor(this, divisor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the ceiling of the result of this FloatStruct with the provided {@code divisor}.
	 */
	@Override
	public QuotientRemainderResult ceiling(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.ceiling(this, divisor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the ceiling of the result of this FloatStruct with the provided {@code divisor}, returning the
	 * quotient as a FloatStruct.
	 */
	@Override
	public QuotientRemainderResult fceiling(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.fceiling(this, divisor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the rounded result of this FloatStruct with the provided {@code divisor}.
	 */
	@Override
	public QuotientRemainderResult round(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.round(this, divisor);
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * Computes the rounded result of this FloatStruct with the provided {@code divisor}, returning the quotient as a
	 * FloatStruct.
	 */
	@Override
	public QuotientRemainderResult fround(final RealStruct divisor) {
		return FloatQuotientRemainderStrategy.INSTANCE.fround(this, divisor);
	}

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * calculation for these values are based on the decoding for Java {@link Double} values from the algorithm defined
	 * in {@link Double#longBitsToDouble}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	public DecodeFloatResult decodeFloat() {
//		if (ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(ZERO, IntegerStruct.ZERO, ONE);
//		}
//		if (MINUS_ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(ZERO, IntegerStruct.ZERO, MINUS_ONE);
//		}

		final int decodedExponentDiffer = 1075;
		final int doubleFloatingPointPrecision = 53;

		final long bits = Double.doubleToRawLongBits(apfloatValue().doubleValue());
		final DecodedDoubleRaw decodedDoubleRaw = getDecodedDoubleRaw(bits);

		final long mantissa = decodedDoubleRaw.getMantissa();
		final BigDecimal mantissaBigDecimal = BigDecimal.valueOf(mantissa);

		final double expt = FastMath.pow(2, doubleFloatingPointPrecision);
		final BigDecimal exptBigDecimal = BigDecimal.valueOf(expt);

		final BigDecimal significand = mantissaBigDecimal.divide(exptBigDecimal, MathContext.DECIMAL128);
		final FloatStruct significandFloat = new FloatStruct(significand);

		final long storedExponent = decodedDoubleRaw.getStoredExponent();
		final long exponent = (storedExponent - decodedExponentDiffer) + doubleFloatingPointPrecision;
		final BigInteger exponentBigInteger = BigInteger.valueOf(exponent);
		final IntegerStruct exponentInteger = new IntegerStruct(exponentBigInteger);

		final long sign = decodedDoubleRaw.getSign();
		final BigDecimal signBigDecimal = BigDecimal.valueOf(sign);
		final FloatStruct signFloat = new FloatStruct(signBigDecimal);

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	/**
	 * @param scale
	 *
	 * @return
	 */
	public NumberStruct scaleFloat(final IntegerStruct scale) {
		final IntegerStruct radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	/**
	 * @return
	 */
	public IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	public FloatStruct floatSign() {
		return floatSign(ONE);
	}

	public FloatStruct floatSign(final FloatStruct float2) {
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				final BigDecimal subtract = BigDecimal.ZERO.subtract(float2.bigDecimal);
				return new FloatStruct(subtract);
			}
		} else {
			return (FloatStruct) float2.abs();
		}
	}

	public IntegerStruct floatDigits() {
		return floatPrecision();
	}

	/**
	 * http://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format
	 *
	 * @return
	 */
	public IntegerStruct floatPrecision() {
		final int binary128Precision = 113;
		return new IntegerStruct(BigInteger.valueOf(binary128Precision));
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
	public DecodeFloatResult integerDecodeFloat() {
//		if (ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(IntegerStruct.ZERO, IntegerStruct.ZERO, IntegerStruct.ONE);
//		}
//		if (MINUS_ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(IntegerStruct.ZERO, IntegerStruct.ZERO, IntegerStruct.MINUS_ONE);
//		}

		final int decodedExponentDiffer = 1075;

		final long bits = Double.doubleToRawLongBits(apfloatValue().doubleValue());
		final DecodedDoubleRaw decodedDoubleRaw = getDecodedDoubleRaw(bits);

		final long mantissa = decodedDoubleRaw.getMantissa();
		final BigInteger mantissaBigInteger = BigInteger.valueOf(mantissa);
		final IntegerStruct significandInteger = new IntegerStruct(mantissaBigInteger);

		final long storedExponent = decodedDoubleRaw.getStoredExponent();
		final long exponent = storedExponent - decodedExponentDiffer;
		final BigInteger exponentBigInteger = BigInteger.valueOf(exponent);
		final IntegerStruct exponentInteger = new IntegerStruct(exponentBigInteger);

		final long sign = decodedDoubleRaw.getSign();
		final BigInteger signBigInteger = BigInteger.valueOf(sign);
		final IntegerStruct signInteger = new IntegerStruct(signBigInteger);

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	/**
	 * See {@link https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html} for details.
	 * <p>
	 * The following is per the JVM spec section 4.4.5
	 *
	 * @return
	 */
	@SuppressWarnings("all")
	private static DecodedDoubleRaw getDecodedDoubleRaw(final long bits) {
		final long sign = ((bits >> 63) == 0) ? 1 : -1;
		final long exponent = (bits >> 52) & 0x7ffL;
		final long mantissa;
		if (exponent == 0) {
			mantissa = (bits & 0xfffffffffffffL) << 1;
		} else {
			mantissa = (bits & 0xfffffffffffffL) | 0x10000000000000L;
		}
		return new DecodedDoubleRaw(mantissa, exponent, sign);
	}

	private static class DecodedDoubleRaw {

		private final long mantissa;

		private final long storedExponent;

		private final long sign;

		private DecodedDoubleRaw(final long mantissa, final long storedExponent, final long sign) {
			this.mantissa = mantissa;
			this.storedExponent = storedExponent;
			this.sign = sign;
		}

		private long getMantissa() {
			return mantissa;
		}

		private long getStoredExponent() {
			return storedExponent;
		}

		private long getSign() {
			return sign;
		}
	}

	// Strategy Implementations

	/**
	 * {@link RealAddStrategy} for computing addition results for {@link FloatStruct}s.
	 */
	private static class FloatAddStrategy extends RealAddStrategy<FloatStruct> {

		/**
		 * Singleton instance of the {@link FloatAddStrategy} type.
		 */
		private static final FloatAddStrategy INSTANCE = new FloatAddStrategy();

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link FloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct add(final FloatStruct number1, final IntegerStruct number2) {
			return addFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link FloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct add(final FloatStruct number1, final RatioStruct number2) {
			return addFloat(number1, number2);
		}
	}

	/**
	 * {@link RealSubtractStrategy} for computing subtraction function results for {@link FloatStruct}s.
	 */
	private static class FloatSubtractStrategy extends RealSubtractStrategy<FloatStruct> {

		/**
		 * Singleton instance of the {@link FloatSubtractStrategy} type.
		 */
		private static final FloatSubtractStrategy INSTANCE = new FloatSubtractStrategy();

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link FloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct subtract(final FloatStruct number1, final IntegerStruct number2) {
			return subtractFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link FloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct subtract(final FloatStruct number1, final RatioStruct number2) {
			return subtractFloat(number1, number2);
		}

		/**
		 * Computes the subtraction for the provided {@link FloatStruct} and {@link RealStruct} using {@link
		 * BigDecimal#subtract(BigDecimal)} with the {@link RealStruct#bigDecimalValue()} values.
		 *
		 * @param number1
		 * 		the {@link FloatStruct} as the first argument of the subtraction operation
		 * @param number2
		 * 		the {@link RealStruct} as the second argument of the subtraction operation
		 *
		 * @return a new {@link FloatStruct} as the result of the subtraction operation
		 */
		private static RealStruct subtractFloat(final FloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new FloatStruct(subtract);
		}
	}

	/**
	 * {@link RealMultiplyStrategy} for computing multiplication function results for {@link FloatStruct}s.
	 */
	private static class FloatMultiplyStrategy extends RealMultiplyStrategy<FloatStruct> {

		/**
		 * Singleton instance of the {@link FloatMultiplyStrategy} type.
		 */
		private static final FloatMultiplyStrategy INSTANCE = new FloatMultiplyStrategy();

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link FloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct multiply(final FloatStruct number1, final IntegerStruct number2) {
			return multiplyFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link FloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct multiply(final FloatStruct number1, final RatioStruct number2) {
			return multiplyFloat(number1, number2);
		}
	}

	/**
	 * {@link RealDivideStrategy} for computing division function results for {@link FloatStruct}s.
	 */
	private static class FloatDivideStrategy extends RealDivideStrategy<FloatStruct> {

		/**
		 * Singleton instance of the {@link FloatDivideStrategy} type.
		 */
		private static final FloatDivideStrategy INSTANCE = new FloatDivideStrategy();

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link FloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct divide(final FloatStruct number1, final IntegerStruct number2) {
			return divideFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link FloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct divide(final FloatStruct number1, final RatioStruct number2) {
			return divideFloat(number1, number2);
		}

		/**
		 * Computes the division for the provided {@link FloatStruct} and {@link RealStruct} using {@link
		 * BigDecimal#divide(BigDecimal, MathContext)} with the {@link RealStruct#bigDecimalValue()} values.
		 *
		 * @param number1
		 * 		the {@link FloatStruct} as the first argument of the division operation
		 * @param number2
		 * 		the {@link RealStruct} as the second argument of the division operation
		 *
		 * @return a new {@link FloatStruct} as the result of the division operation
		 */
		private static RealStruct divideFloat(final FloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new FloatStruct(divide);
		}
	}

	/**
	 * {@link FloatQuotientRemainderStrategy} for computing quotient and remainder results for {@link
	 * FloatStruct}s.
	 */
	private static class FloatQuotientRemainderStrategy extends QuotientRemainderStrategy<FloatStruct> {

		/**
		 * Singleton instance of the {@link FloatQuotientRemainderStrategy} type.
		 */
		private static final FloatQuotientRemainderStrategy INSTANCE = new FloatQuotientRemainderStrategy();

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for a {@link FloatStruct} as the {@code real} and an {@link
		 * IntegerStruct} as the {@code divisor}.
		 */
		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct real, final IntegerStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return floatQuotientRemainder(real, divisor, roundingMode, isQuotientFloat);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the quotient and remainder results for a {@link FloatStruct} as the {@code real} and an {@link
		 * RatioStruct} as the {@code divisor}.
		 */
		@Override
		public QuotientRemainderResult quotientRemainder(final FloatStruct real, final RatioStruct divisor,
		                                                 final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			return floatQuotientRemainder(real, divisor, roundingMode, isQuotientFloat);
		}
	}

	// HashCode / Equals

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigDecimal)
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
		final FloatStruct rhs = (FloatStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigDecimal, rhs.bigDecimal)
		                          .isEquals();
	}
}
