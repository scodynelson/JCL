/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.MathContext;

import jcl.classes.BuiltInClassStruct;
import jcl.types.DoubleFloatType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link DoubleFloatStruct} is the object representation of a Lisp 'float' type.
 */
public final class DoubleFloatStruct extends BuiltInClassStruct implements FloatStruct {

	/**
	 * {@link DoubleFloatStruct} constant representing 0.0.
	 */
	public static final DoubleFloatStruct ZERO = valueOf(0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -0.0.
	 */
	public static final DoubleFloatStruct MINUS_ZERO = valueOf(-0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing 1.0.
	 */
	public static final DoubleFloatStruct ONE = valueOf(1.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -1.0.
	 */
	public static final DoubleFloatStruct MINUS_ONE = valueOf(-1.0D);

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(DoubleFloatStruct.class);

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
		if (LOGGER.isWarnEnabled()) {
			LOGGER.warn("Possible loss of precision.");
		}
		return NumberUtils.doubleToFloat(d);
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
	public RealStruct.FloatingPointVisitor<?> floatingPointVisitor() {
		return new DoubleFloatFloatingPointVisitor(this);
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final int expt = ArithmeticUtils.pow(2, DOUBLE_PRECISION);
		final long significand = mantissa / expt;
		final DoubleFloatStruct significandFloat = new DoubleFloatStruct(significand);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = (storedExponent - 1075) + DOUBLE_PRECISION;
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponent);

		final int sign = decodedDouble.getSign();
		final DoubleFloatStruct signFloat = (sign == 1) ? ONE : MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final IntegerStruct significandInteger = IntegerStruct.valueOf(mantissa);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = storedExponent - 1075;
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponent);

		final int sign = decodedDouble.getSign();
		final IntegerStruct signInteger = (sign == 1) ? IntegerStruct.ONE : IntegerStruct.MINUS_ONE;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.valueOf(DOUBLE_PRECISION);
	}

	@Override
	public FloatStruct floatSign() {
		final long bits = Double.doubleToRawLongBits(d);
		return (bits < 0) ? MINUS_ONE : ONE;
	}

	/**
	 * Decodes the double by the provided {@code long} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 *
	 * @param bits
	 * 		the {@code long} bits representing the {@code double} value
	 *
	 * @return the {@link DecodedDouble} wrapping the decoded sign, exponent, and mantissa values
	 *
	 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html">Java Double</a>
	 */
	private static DecodedDouble getDecodedDouble(final long bits) {
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
	public RealStruct.LessThanVisitor<?> lessThanVisitor() {
		return new DoubleFloatLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new DoubleFloatGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new DoubleFloatLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new DoubleFloatGreaterThanOrEqualToVisitor(this);
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
	public EqualToVisitor<?> equalToVisitor() {
		return new DoubleFloatEqualToVisitor(this);
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
		return new DoubleFloatExptVisitor(this);
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
		return negation();
	}

	@Override
	public DoubleFloatStruct negation() {
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
			final double d1 = number1.d;
			final int i = number2.i;
			return valueOf(d1 + i);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final double d1 = number1.d;
			final long l = number2.l;
			return valueOf(d1 + l);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigInteger.doubleValue();
			return valueOf(d1 + d2);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final double d1 = number1.d;
			final float f = number2.f;
			return valueOf(d1 + f);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.d;
			return valueOf(d1 + d2);
		}

		@Override
		public RealStruct add(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return BigFloatStruct.valueOf(add);
		}

		@Override
		public RealStruct add(final RatioStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigFraction.doubleValue();
			return valueOf(d1 + d2);
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
			final double d1 = number1.d;
			final int i = number2.i;
			return valueOf(d1 - i);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final double d1 = number1.d;
			final long l = number2.l;
			return valueOf(d1 - l);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigInteger.doubleValue();
			return valueOf(d1 - d2);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final double d1 = number1.d;
			final float f = number2.f;
			return valueOf(d1 - f);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.d;
			return valueOf(d1 - d2);
		}

		@Override
		public RealStruct subtract(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return BigFloatStruct.valueOf(subtract);
		}

		@Override
		public RealStruct subtract(final RatioStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigFraction.doubleValue();
			return valueOf(d1 - d2);
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
			final double d1 = number1.d;
			final int i = number2.i;
			return valueOf(d1 * i);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final double d1 = number1.d;
			final long l = number2.l;
			return valueOf(d1 * l);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigInteger.doubleValue();
			return valueOf(d1 * d2);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final double d1 = number1.d;
			final float f = number2.f;
			return valueOf(d1 * f);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.d;
			return valueOf(d1 * d2);
		}

		@Override
		public RealStruct multiply(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return BigFloatStruct.valueOf(multiply);
		}

		@Override
		public RealStruct multiply(final RatioStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigFraction.doubleValue();
			return valueOf(d1 * d2);
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
			final double d1 = number1.d;
			final int i = number2.i;
			return valueOf(d1 / i);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final double d1 = number1.d;
			final long l = number2.l;
			return valueOf(d1 / l);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigInteger.doubleValue();
			return valueOf(d1 / d2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final double d1 = number1.d;
			final float f = number2.f;
			return valueOf(d1 / f);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.d;
			return valueOf(d1 / d2);
		}

		@Override
		public RealStruct divide(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL128);
			return BigFloatStruct.valueOf(divide);
		}

		@Override
		public RealStruct divide(final RatioStruct number2) {
			final double d1 = number1.d;
			final double d2 = number2.bigFraction.doubleValue();
			return valueOf(d1 / d2);
		}
	}

	/**
	 * {@link FloatStruct.FloatEqualToVisitor} for computing numeric '=' equality results for {@link
	 * DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatEqualToVisitor extends FloatStruct.FloatEqualToVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatEqualToVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private DoubleFloatEqualToVisitor(final DoubleFloatStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			return Double.compare(number1.d, number2.f) == 0;
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			return Double.compare(number1.d, number2.d) == 0;
		}

		@Override
		public boolean equalTo(final BigFloatStruct number2) {
			final BigDecimal bigDecimal1 = number1.bigDecimalValue();
			final BigDecimal bigDecimal2 = number2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) == 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatLessThanVisitor} for computing numeric {@literal '<'} equality results for {@link
	 * DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatLessThanVisitor extends FloatStruct.FloatLessThanVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatLessThanVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private DoubleFloatLessThanVisitor(final DoubleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			return Double.compare(real1.d, real2.f) < 0;
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			return Double.compare(real1.d, real2.d) < 0;
		}

		@Override
		public boolean lessThan(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) < 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatGreaterThanVisitor} for computing numeric {@literal '>'} equality results for {@link
	 * DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatGreaterThanVisitor extends FloatStruct.FloatGreaterThanVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatGreaterThanVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private DoubleFloatGreaterThanVisitor(final DoubleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			return Double.compare(real1.d, real2.f) > 0;
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			return Double.compare(real1.d, real2.d) > 0;
		}

		@Override
		public boolean greaterThan(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) > 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatLessThanOrEqualToVisitor} for computing numeric {@literal '<='} equality results for
	 * {@link DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatLessThanOrEqualToVisitor extends FloatStruct.FloatLessThanOrEqualToVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatLessThanOrEqualToVisitor with the provided
		 * {@link DoubleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private DoubleFloatLessThanOrEqualToVisitor(final DoubleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			return Double.compare(real1.d, real2.f) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			return Double.compare(real1.d, real2.d) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) <= 0;
		}
	}

	/**
	 * {@link FloatStruct.FloatGreaterThanOrEqualToVisitor} for computing numeric {@literal '>='} equality results for
	 * {@link DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatGreaterThanOrEqualToVisitor extends FloatStruct.FloatGreaterThanOrEqualToVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatGreaterThanOrEqualToVisitor with the provided
		 * {@link DoubleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private DoubleFloatGreaterThanOrEqualToVisitor(final DoubleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			return Double.compare(real1.d, real2.f) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			return Double.compare(real1.d, real2.d) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link DoubleFloatStruct}s.
	 */
	private static final class DoubleFloatExptVisitor extends RealStruct.RealExptVisitor<DoubleFloatStruct> {
		// TODO: fix

		/**
		 * Private constructor to make a new instance of an DoubleFloatExptVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private DoubleFloatExptVisitor(final DoubleFloatStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.i);
		}

		@Override
		@SuppressWarnings("deprecation")
		public NumberStruct expt(final LongIntegerStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.l);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.bigInteger.doubleValue());
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.f);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.d);
		}

		@Override
		public NumberStruct expt(final BigFloatStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.doubleValue());
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			// TODO: more efficient?
			return exptFloatRatioNew(base.d, power.bigFraction.doubleValue());
		}
	}

	/**
	 * {@link RealStruct.FloatingPointVisitor} for converting a {@link RealStruct} to the {@link DoubleFloatStruct}
	 * type of {@link #prototype}.
	 */
	private static final class DoubleFloatFloatingPointVisitor extends RealStruct.FloatingPointVisitor<DoubleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an DoubleFloatFloatingPointVisitor with the provided {@link
		 * DoubleFloatStruct}.
		 *
		 * @param prototype
		 * 		the prototype argument in the floating-point conversion operation
		 */
		private DoubleFloatFloatingPointVisitor(final DoubleFloatStruct prototype) {
			super(prototype);
		}

		@Override
		public FloatStruct floatingPoint(final IntIntegerStruct real) {
			return valueOf(real.i);
		}

		@Override
		public FloatStruct floatingPoint(final LongIntegerStruct real) {
			return valueOf(real.l);
		}

		@Override
		public FloatStruct floatingPoint(final BigIntegerStruct real) {
			return valueOf(real.bigInteger.doubleValue());
		}

		@Override
		public FloatStruct floatingPoint(final SingleFloatStruct real) {
			return valueOf(real.f);
		}

		@Override
		public FloatStruct floatingPoint(final DoubleFloatStruct real) {
			return real;
		}

		@Override
		public FloatStruct floatingPoint(final BigFloatStruct real) {
			return valueOf(real.bigDecimal.doubleValue());
		}

		@Override
		public FloatStruct floatingPoint(final RatioStruct real) {
			return valueOf(real.bigFraction.doubleValue());
		}
	}
}
