/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import jcl.classes.BuiltInClassStruct;
import jcl.types.DoubleFloatType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link DoubleFloatStruct} is the object representation of a Lisp 'float' type.
 */
public final class DoubleFloatStruct extends BuiltInClassStruct implements FloatStruct {

	/**
	 * {@link DoubleFloatStruct} constant representing 0.0.
	 */
	public static final DoubleFloatStruct ZERO = new DoubleFloatStruct(0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -0.0.
	 */
	public static final DoubleFloatStruct MINUS_ZERO = new DoubleFloatStruct(-0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing 1.0.
	 */
	public static final DoubleFloatStruct ONE = new DoubleFloatStruct(1.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -1.0.
	 */
	public static final DoubleFloatStruct MINUS_ONE = new DoubleFloatStruct(-1.0D);

	/**
	 * The floating-point precision of a DoubleFloatStruct object.
	 */
	private static final int DOUBLE_PRECISION = 53;

	/**
	 * The internal {@code double} containing the DoubleFloatStruct contents.
	 */
	final double d;

	/**
	 * Private constructor.
	 *
	 * @param d
	 * 		the value of the DoubleFloatStruct
	 */
	private DoubleFloatStruct(final double d) {
		super(DoubleFloatType.INSTANCE, null, null);
		this.d = d;
	}

	/**
	 * Returns a DoubleFloatStruct object with the provided {@code double} value.
	 *
	 * @param d
	 * 		the {@code double} value of the resulting DoubleFloatStruct
	 *
	 * @return a DoubleFloatStruct object with the provided {@code double} value
	 */
	public static DoubleFloatStruct valueOf(final double d) {
		return new DoubleFloatStruct(d);
	}

	/*
		FloatStruct
	 */

	@Override
	public float floatValue() {
		// TODO: Warn loss of precision
		return Double.valueOf(d).floatValue();
	}

	@Override
	public double doubleValue() {
		return d;
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return NumberUtils.bigDecimalValue(d);
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final int decodedExponentDiffer = 1075;
		final int doubleFloatingPointPrecision = 53;

		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDoubleRaw = getDecodedDoubleRaw(bits);

		final long mantissa = decodedDoubleRaw.getMantissa();
		final BigDecimal mantissaBigDecimal = NumberUtils.bigDecimalValue(mantissa);

		final double expt = StrictMath.pow(2, doubleFloatingPointPrecision);
		final BigDecimal exptBigDecimal = NumberUtils.bigDecimalValue(expt);

		final BigDecimal significand = mantissaBigDecimal.divide(exptBigDecimal, MathContext.DECIMAL128);
		final DoubleFloatStruct significandFloat = new DoubleFloatStruct(significand.doubleValue());

		final long storedExponent = decodedDoubleRaw.getStoredExponent();
		final long exponent = (storedExponent - decodedExponentDiffer) + doubleFloatingPointPrecision;
		final BigInteger exponentBigInteger = BigInteger.valueOf(exponent);
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponentBigInteger);

		final long sign = decodedDoubleRaw.getSign();
		final BigDecimal signBigDecimal = NumberUtils.bigDecimalValue(sign);
		final DoubleFloatStruct signFloat = new DoubleFloatStruct(signBigDecimal.doubleValue());

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final int decodedExponentDiffer = 1075;

		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDoubleRaw = getDecodedDoubleRaw(bits);

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

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.valueOf(DOUBLE_PRECISION);
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
				return new DoubleFloatStruct(subtract.doubleValue());
			}
		} else {
			return (DoubleFloatStruct) float2.abs();
		}
	}

	/**
	 * Decodes the float by the provided {@code long} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 * TODO: check spec section
	 *
	 * @param bits
	 * 		the {@code long} bits representing the {@code double} value
	 *
	 * @return the {@link DecodedDouble} wrapping the decoded sign, exponent, and mantissa values
	 *
	 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html">Java Double</a>
	 */
	private static DecodedDouble getDecodedDoubleRaw(final long bits) {
		final int sign = ((bits >> 63) == 0) ? 1 : -1;
		final long exponent = (bits >> 52) & 0x7ffL;
		final long mantissa = (exponent == 0) ?
		                      ((bits & 0xfffffffffffffL) << 1) :
		                      ((bits & 0xfffffffffffffL) | 0x10000000000000L);
		return new DecodedDouble(mantissa, exponent, sign);
	}

	/**
	 * Decoded wrapper for {@code double} sign, exponent, and mantissa values.
	 */
	private static final class DecodedDouble {

		/**
		 * The part of the {@code double} that represents the significant digits.
		 */
		private final long mantissa;

		/**
		 * The part of the {@code double} that represents the exponent.
		 */
		private final long storedExponent;

		/**
		 * The part of the {@code double} that represents the sign bit.
		 */
		private final int sign;

		/**
		 * Private constructor.
		 *
		 * @param mantissa
		 * 		the part of the {@code double} that represents the significant digits
		 * @param storedExponent
		 * 		the part of the {@code double} that represents the exponent
		 * @param sign
		 * 		the part of the {@code double} that represents the sign bit
		 */
		private DecodedDouble(final long mantissa, final long storedExponent, final int sign) {
			this.mantissa = mantissa;
			this.storedExponent = storedExponent;
			this.sign = sign;
		}

		/**
		 * Getter for {@link #mantissa} property value.
		 *
		 * @return {@link #mantissa} property value
		 */
		private long getMantissa() {
			return mantissa;
		}

		/**
		 * Getter for {@link #storedExponent} property value.
		 *
		 * @return {@link #storedExponent} property value
		 */
		private long getStoredExponent() {
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
		return d > 0.0D;
	}

	@Override
	public boolean minusp() {
		return d < 0.0D;
	}

	@Override
	public RationalStruct rational() {
		final BigFraction bigFraction = new BigFraction(d);
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
		return new DoubleFloatAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new DoubleFloatSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new DoubleFloatMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new DoubleFloatDivideVisitor(this);
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
		return d == 0.0D;
	}

	@Override
	public RealStruct abs() {
		if (d >= 0.0D) {
			return this;
		}
		return new DoubleFloatStruct(-d);
	}

	@Override
	public NumberStruct negation() {
		if (isEqualTo(ZERO)) {
			return MINUS_ZERO;
		}
		if (isEqualTo(MINUS_ZERO)) {
			return ZERO;
		}
		return new DoubleFloatStruct(-d);
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
		return new HashCodeBuilder().append(d)
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
		final DoubleFloatStruct rhs = (DoubleFloatStruct) obj;
		return new EqualsBuilder().append(d, rhs.d)
		                          .isEquals();
	}

	// Visitor Implementations

	/**
	 * {@link RealStruct.RealAddVisitor} for computing addition results for {@link DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatAddVisitor extends RealStruct.RealAddVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatAddVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private DoubleFloatAddVisitor(final DoubleFloatStruct number1) {
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

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			return super.add(number2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link
	 * DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatSubtractVisitor extends RealStruct.RealSubtractVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatSubtractVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private DoubleFloatSubtractVisitor(final DoubleFloatStruct number1) {
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

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			return super.subtract(number2);
		}

		private static RealStruct subtractFloat(final DoubleFloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return new DoubleFloatStruct(subtract.doubleValue());
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatMultiplyVisitor extends RealStruct.RealMultiplyVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatMultiplyVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private DoubleFloatMultiplyVisitor(final DoubleFloatStruct number1) {
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

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			return super.multiply(number2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatDivideVisitor extends RealStruct.RealDivideVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatDivideVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private DoubleFloatDivideVisitor(final DoubleFloatStruct number1) {
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

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			return super.divide(number2);
		}

		private static RealStruct divideFloat(final DoubleFloatStruct number1, final RealStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimalValue();
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return new DoubleFloatStruct(divide.doubleValue());
		}
	}
}
