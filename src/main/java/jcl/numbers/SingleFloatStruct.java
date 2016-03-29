/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import jcl.classes.BuiltInClassStruct;
import jcl.types.SingleFloatType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link SingleFloatStruct} is the object representation of a Lisp 'float' type.
 */
public final class SingleFloatStruct extends BuiltInClassStruct implements FloatStruct {

	/**
	 * {@link SingleFloatStruct} constant representing 0.0.
	 */
	public static final SingleFloatStruct ZERO = valueOf(0.0F);

	/**
	 * {@link SingleFloatStruct} constant representing -0.0.
	 */
	public static final SingleFloatStruct MINUS_ZERO = valueOf(-0.0F);

	/**
	 * {@link SingleFloatStruct} constant representing 1.0.
	 */
	public static final SingleFloatStruct ONE = valueOf(1.0F);

	/**
	 * {@link SingleFloatStruct} constant representing -1.0.
	 */
	public static final SingleFloatStruct MINUS_ONE = valueOf(-1.0F);

	/**
	 * The internal {@code float} containing the SingleFloatStruct contents.
	 */
	final float f;

	/**
	 * Private constructor.
	 *
	 * @param f
	 * 		the value of the SingleFloatStruct
	 */
	private SingleFloatStruct(final float f) {
		super(SingleFloatType.INSTANCE, null, null);
		this.f = f;
	}

	/**
	 * Returns a SingleFloatStruct object with the provided {@code float} value.
	 *
	 * @param f
	 * 		the {@code float} value of the resulting SingleFloatStruct
	 *
	 * @return a SingleFloatStruct object with the provided {@code float} value
	 */
	public static SingleFloatStruct valueOf(final float f) {
		return new SingleFloatStruct(f);
	}

	/*
		FloatStruct
	 */

	@Override
	public float floatValue() {
		return f;
	}

	@Override
	public double doubleValue() {
		return f;
	}

	@Override
	public BigDecimal bigDecimalValue() {
		// NOTE: Using 'String.valueOf' since the BigDecimal#valueOf(double) does some floating point rounding crap that
		//          changes the actual value since it stores it as a 'long' under the hood.
		return new BigDecimal(String.valueOf(f));
	}

	@Override
	public DecodeFloatResult decodeFloat() {
//		if (ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(ZERO, IntegerStruct.ZERO, ONE);
//		}
//		if (MINUS_ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(ZERO, IntegerStruct.ZERO, MINUS_ONE);
//		}

		final int decodedExponentDiffer = 1075;
		final int doubleFloatingPointPrecision = 53;

		final int bits = Float.floatToRawIntBits(apfloatValue().floatValue());
		final DecodedDoubleRaw decodedDoubleRaw = getDecodedFloatRaw(bits);

		final long mantissa = decodedDoubleRaw.getMantissa();
		final BigDecimal mantissaBigDecimal = BigDecimal.valueOf(mantissa);

		final double expt = StrictMath.pow(2, doubleFloatingPointPrecision);
		final BigDecimal exptBigDecimal = BigDecimal.valueOf(expt);

		final BigDecimal significand = mantissaBigDecimal.divide(exptBigDecimal, MathContext.DECIMAL128);
		final SingleFloatStruct significandFloat = new SingleFloatStruct(significand.floatValue());

		final long storedExponent = decodedDoubleRaw.getStoredExponent();
		final long exponent = (storedExponent - decodedExponentDiffer) + doubleFloatingPointPrecision;
		final BigInteger exponentBigInteger = BigInteger.valueOf(exponent);
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponentBigInteger);

		final long sign = decodedDoubleRaw.getSign();
		final BigDecimal signBigDecimal = BigDecimal.valueOf(sign);
		final SingleFloatStruct signFloat = new SingleFloatStruct(signBigDecimal.floatValue());

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
//		if (ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(IntegerStruct.ZERO, IntegerStruct.ZERO, IntegerStruct.ONE);
//		}
//		if (MINUS_ZERO.isEqualTo(this)) {
//			return new DecodeFloatResult(IntegerStruct.ZERO, IntegerStruct.ZERO, IntegerStruct.MINUS_ONE);
//		}

		final int decodedExponentDiffer = 1075;

		final int bits = Float.floatToRawIntBits(apfloatValue().floatValue());
		final DecodedDoubleRaw decodedDoubleRaw = getDecodedFloatRaw(bits);

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
	 * http://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format
	 */
	@Override
	public IntegerStruct floatPrecision() {
		final int binary32Precision = 24;
//		final int binary64Precision = 53;
//		final int binary128Precision = 113;
		return IntegerStruct.valueOf(BigInteger.valueOf(binary32Precision));
	}

	@Override
	public FloatStruct floatSign() {
		return floatSign(ONE);
	}

	@Override
	public FloatStruct floatSign(final FloatStruct float2) {
		// TODO: Visitor implementations??
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				final BigDecimal subtract = BigDecimal.ZERO.subtract(float2.getBigDecimal());
				return new SingleFloatStruct(subtract.floatValue());
			}
		} else {
			return (SingleFloatStruct) float2.abs();
		}
	}

	/*
	 * See {@link https://docs.oracle.com/javase/8/docs/api/java/lang/Float.html} for details.
	 * <p>
	 * The following is per the JVM spec section 4.4.5
	 */
	@SuppressWarnings("all")
	private static DecodedDoubleRaw getDecodedFloatRaw(final int bits) {
		final int sign = ((bits >> 31) == 0) ? 1 : -1;
		// 0xff == 255 == exponent max
		final int exponent = (bits >> 23) & 0xff;
		final int mantissa;
		if (exponent == 0) {
			// 0x7fffff == 8388607 == 2^23 - 1
			mantissa = (bits & 0x7fffff) << 1;
		} else {
			// 0x7fffff == 8388607 == 2^23 - 1
			// 0x800000 == 8388608 == 2^23
			mantissa = (bits & 0x7fffff) | 0x800000;
		}
		return new DecodedDoubleRaw(mantissa, exponent, sign);
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
		return f > 0.0F;
	}

	@Override
	public boolean minusp() {
		return f < 0.0F;
	}

	@Override
	public RationalStruct rational() {
		final BigFraction bigFraction = new BigFraction(f);
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
		return f == 0.0F;
	}

	@Override
	public RealStruct abs() {
		if (f >= 0.0F) {
			return this;
		}
		return negation();
	}

	@Override
	public SingleFloatStruct negation() {
		if (isEqualTo(ZERO)) {
			return MINUS_ZERO;
		}
		if (isEqualTo(MINUS_ZERO)) {
			return ZERO;
		}
		return new SingleFloatStruct(-f);
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
		return new HashCodeBuilder().append(f)
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
		final SingleFloatStruct rhs = (SingleFloatStruct) obj;
		return new EqualsBuilder().append(f, rhs.f)
		                          .isEquals();
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link SingleFloatStruct}s.
	 */
	private static final class FloatAddVisitor extends RealStruct.RealAddVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an FloatAddVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private FloatAddVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			return addFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			return addFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the addition function result for an {@link SingleFloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct add(final RatioStruct number2) {
			return addFloat(number1, number2);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			return super.add(number2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link
	 * SingleFloatStruct}s.
	 */
	private static final class FloatSubtractVisitor extends RealStruct.RealSubtractVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an FloatSubtractVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private FloatSubtractVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			return subtractFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			return subtractFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the subtraction function result for an {@link SingleFloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct subtract(final RatioStruct number2) {
			return subtractFloat(number1, number2);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			return super.subtract(number2);
		}

		/**
		 * Computes the subtraction for the provided {@link SingleFloatStruct} and {@link RealStruct} using {@link
		 * BigDecimal#subtract(BigDecimal)} with the {@link RealStruct#bigDecimalValue()} values.
		 *
		 * @param number1
		 * 		the {@link SingleFloatStruct} as the first argument of the subtraction operation
		 * @param number2
		 * 		the {@link RealStruct} as the second argument of the subtraction operation
		 *
		 * @return a new {@link SingleFloatStruct} as the result of the subtraction operation
		 */
		private static RealStruct subtractFloat(final SingleFloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new SingleFloatStruct(subtract.floatValue());
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * SingleFloatStruct}s.
	 */
	private static final class FloatMultiplyVisitor extends RealStruct.RealMultiplyVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an FloatMultiplyVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private FloatMultiplyVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			return multiplyFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			return multiplyFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the multiplication function result for an {@link SingleFloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct multiply(final RatioStruct number2) {
			return multiplyFloat(number1, number2);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			return super.multiply(number2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link SingleFloatStruct}s.
	 */
	private static final class FloatDivideVisitor extends RealStruct.RealDivideVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an FloatDivideVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private FloatDivideVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			return divideFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			return divideFloat(number1, number2);
		}

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link SingleFloatStruct} and a {@link IntegerStruct}.
		 */
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

		/**
		 * {@inheritDoc}
		 * <p>
		 * Computes the division function result for an {@link SingleFloatStruct} and a {@link RatioStruct}.
		 */
		@Override
		public RealStruct divide(final RatioStruct number2) {
			return divideFloat(number1, number2);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			return super.divide(number2);
		}

		/**
		 * Computes the division for the provided {@link SingleFloatStruct} and {@link RealStruct} using {@link
		 * BigDecimal#divide(BigDecimal, MathContext)} with the {@link RealStruct#bigDecimalValue()} values.
		 *
		 * @param number1
		 * 		the {@link SingleFloatStruct} as the first argument of the division operation
		 * @param number2
		 * 		the {@link RealStruct} as the second argument of the division operation
		 *
		 * @return a new {@link SingleFloatStruct} as the result of the division operation
		 */
		private static RealStruct divideFloat(final SingleFloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new SingleFloatStruct(divide.floatValue());
		}
	}
}
