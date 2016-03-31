/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.MathContext;

import jcl.classes.BuiltInClassStruct;
import jcl.types.SingleFloatType;
import jcl.util.NumberUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;

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
	 * The floating-point precision of a SingleFloatStruct object.
	 */
	private static final int FLOAT_PRECISION = 24;

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
		return NumberUtils.bigDecimalValue(f);
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final int bits = Float.floatToRawIntBits(f);
		final DecodedFloat decodedFloat = getDecodedFloat(bits);

		final int mantissa = decodedFloat.getMantissa();
		final int expt = ArithmeticUtils.pow(2, FLOAT_PRECISION);
		final int significand = mantissa / expt;
		final SingleFloatStruct significandFloat = new SingleFloatStruct(significand);

		final int storedExponent = decodedFloat.getStoredExponent();
		final int exponent = (storedExponent - 150) + FLOAT_PRECISION;
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponent);

		final int sign = decodedFloat.getSign();
		final SingleFloatStruct signFloat = (sign == 1) ? ONE : MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final int bits = Float.floatToRawIntBits(f);
		final DecodedFloat decodedFloat = getDecodedFloat(bits);

		final int mantissa = decodedFloat.getMantissa();
		final IntegerStruct significandInteger = IntegerStruct.valueOf(mantissa);

		final int storedExponent = decodedFloat.getStoredExponent();
		final int exponent = storedExponent - 150;
		final IntegerStruct exponentInteger = IntegerStruct.valueOf(exponent);

		final int sign = decodedFloat.getSign();
		final IntegerStruct signInteger = (sign == 1) ? IntegerStruct.ONE : IntegerStruct.MINUS_ONE;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.valueOf(FLOAT_PRECISION);
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

	/**
	 * Decodes the float by the provided {@code int} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 * TODO: check spec section
	 *
	 * @param bits
	 * 		the {@code int} bits representing the {@code float} value
	 *
	 * @return the {@link DecodedFloat} wrapping the decoded sign, exponent, and mantissa values
	 *
	 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/Float.html">Java Float</a>
	 */
	private static DecodedFloat getDecodedFloat(final int bits) {
		final int sign = ((bits >> 31) == 0) ? 1 : -1;
		final int exponent = (bits >> 23) & 0xff;
		final int mantissa = (exponent == 0) ?
		                     ((bits & 0x7fffff) << 1) :
		                     ((bits & 0x7fffff) | 0x800000);
		return new DecodedFloat(mantissa, exponent, sign);
	}

	/**
	 * Decoded wrapper for {@code float} sign, exponent, and mantissa values.
	 */
	private static final class DecodedFloat {

		/**
		 * The part of the {@code float} that represents the significant digits.
		 */
		private final int mantissa;

		/**
		 * The part of the {@code float} that represents the exponent.
		 */
		private final int storedExponent;

		/**
		 * The part of the {@code float} that represents the sign bit.
		 */
		private final int sign;

		/**
		 * Private constructor.
		 *
		 * @param mantissa
		 * 		the part of the {@code float} that represents the significant digits
		 * @param storedExponent
		 * 		the part of the {@code float} that represents the exponent
		 * @param sign
		 * 		the part of the {@code float} that represents the sign bit
		 */
		private DecodedFloat(final int mantissa, final int storedExponent, final int sign) {
			this.mantissa = mantissa;
			this.storedExponent = storedExponent;
			this.sign = sign;
		}

		/**
		 * Getter for {@link #mantissa} property value.
		 *
		 * @return {@link #mantissa} property value
		 */
		private int getMantissa() {
			return mantissa;
		}

		/**
		 * Getter for {@link #storedExponent} property value.
		 *
		 * @return {@link #storedExponent} property value
		 */
		private int getStoredExponent() {
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
		return new SingleFloatLessThanVisitor(this);
	}

	@Override
	public boolean isGreaterThan(final GreaterThanVisitor<?> greaterThanVisitor) {
		return greaterThanVisitor.greaterThan(this);
	}

	@Override
	public RealStruct.GreaterThanVisitor<?> greaterThanVisitor() {
		return new SingleFloatGreaterThanVisitor(this);
	}

	@Override
	public boolean isLessThanOrEqualTo(final LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor) {
		return lessThanOrEqualToVisitor.lessThanOrEqualTo(this);
	}

	@Override
	public RealStruct.LessThanOrEqualToVisitor<?> lessThanOrEqualToVisitor() {
		return new SingleFloatLessThanOrEqualToVisitor(this);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor) {
		return greaterThanOrEqualToVisitor.greaterThanOrEqualTo(this);
	}

	@Override
	public RealStruct.GreaterThanOrEqualToVisitor<?> greaterThanOrEqualToVisitor() {
		return new SingleFloatGreaterThanOrEqualToVisitor(this);
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
		return new SingleFloatAddVisitor(this);
	}

	@Override
	public NumberStruct subtract(final SubtractVisitor<?> subtractVisitor) {
		return subtractVisitor.subtract(this);
	}

	@Override
	public SubtractVisitor<?> subtractVisitor() {
		return new SingleFloatSubtractVisitor(this);
	}

	@Override
	public NumberStruct multiply(final MultiplyVisitor<?> multiplyVisitor) {
		return multiplyVisitor.multiply(this);
	}

	@Override
	public MultiplyVisitor<?> multiplyVisitor() {
		return new SingleFloatMultiplyVisitor(this);
	}

	@Override
	public NumberStruct divide(final DivideVisitor<?> divideVisitor) {
		return divideVisitor.divide(this);
	}

	@Override
	public DivideVisitor<?> divideVisitor() {
		return new SingleFloatDivideVisitor(this);
	}

	@Override
	public boolean isEqualTo(final EqualToVisitor<?> equalToVisitor) {
		return equalToVisitor.equalTo(this);
	}

	@Override
	public EqualToVisitor<?> equalToVisitor() {
		return new SingleFloatEqualToVisitor(this);
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
	private static final class SingleFloatAddVisitor extends RealStruct.RealAddVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatAddVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the addition operation
		 */
		private SingleFloatAddVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct add(final IntIntegerStruct number2) {
			final float f1 = number1.f;
			final int i = number2.i;
			return valueOf(f1 + i);
		}

		@Override
		public RealStruct add(final LongIntegerStruct number2) {
			final float f1 = number1.f;
			final long l = number2.l;
			return valueOf(f1 + l);
		}

		@Override
		public RealStruct add(final BigIntegerStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigInteger.floatValue();
			return valueOf(f1 + f2);
		}

		@Override
		public RealStruct add(final SingleFloatStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.f;
			return valueOf(f1 + f2);
		}

		@Override
		public RealStruct add(final DoubleFloatStruct number2) {
			final float f1 = number1.f;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(f1 + d);
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
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 + f2);
		}
	}

	/**
	 * {@link RealStruct.RealSubtractVisitor} for computing subtraction function results for {@link
	 * SingleFloatStruct}s.
	 */
	private static final class SingleFloatSubtractVisitor extends RealStruct.RealSubtractVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatSubtractVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the subtraction operation
		 */
		private SingleFloatSubtractVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct subtract(final IntIntegerStruct number2) {
			final float f1 = number1.f;
			final int i = number2.i;
			return valueOf(f1 - i);
		}

		@Override
		public RealStruct subtract(final LongIntegerStruct number2) {
			final float f1 = number1.f;
			final long l = number2.l;
			return valueOf(f1 - l);
		}

		@Override
		public RealStruct subtract(final BigIntegerStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigInteger.floatValue();
			return valueOf(f1 - f2);
		}

		@Override
		public RealStruct subtract(final SingleFloatStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.f;
			return valueOf(f1 - f2);
		}

		@Override
		public RealStruct subtract(final DoubleFloatStruct number2) {
			final float f1 = number1.f;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(f1 - d);
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
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 - f2);
		}
	}

	/**
	 * {@link RealStruct.RealMultiplyVisitor} for computing multiplication function results for {@link
	 * SingleFloatStruct}s.
	 */
	private static final class SingleFloatMultiplyVisitor extends RealStruct.RealMultiplyVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatMultiplyVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the multiplication operation
		 */
		private SingleFloatMultiplyVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct multiply(final IntIntegerStruct number2) {
			final float f1 = number1.f;
			final int i = number2.i;
			return valueOf(f1 * i);
		}

		@Override
		public RealStruct multiply(final LongIntegerStruct number2) {
			final float f1 = number1.f;
			final long l = number2.l;
			return valueOf(f1 * l);
		}

		@Override
		public RealStruct multiply(final BigIntegerStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigInteger.floatValue();
			return valueOf(f1 * f2);
		}

		@Override
		public RealStruct multiply(final SingleFloatStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.f;
			return valueOf(f1 * f2);
		}

		@Override
		public RealStruct multiply(final DoubleFloatStruct number2) {
			final float f1 = number1.f;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(f1 * d);
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
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 * f2);
		}
	}

	/**
	 * {@link RealStruct.RealDivideVisitor} for computing division function results for {@link SingleFloatStruct}s.
	 */
	private static final class SingleFloatDivideVisitor extends RealStruct.RealDivideVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatDivideVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the division operation
		 */
		private SingleFloatDivideVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		@Override
		public RealStruct divide(final IntIntegerStruct number2) {
			final float f1 = number1.f;
			final int i = number2.i;
			return valueOf(f1 / i);
		}

		@Override
		public RealStruct divide(final LongIntegerStruct number2) {
			final float f1 = number1.f;
			final long l = number2.l;
			return valueOf(f1 / l);
		}

		@Override
		public RealStruct divide(final BigIntegerStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigInteger.floatValue();
			return valueOf(f1 / f2);
		}

		@Override
		public RealStruct divide(final SingleFloatStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.f;
			return valueOf(f1 / f2);
		}

		@Override
		public RealStruct divide(final DoubleFloatStruct number2) {
			final float f1 = number1.f;
			final double d = number2.d;
			return DoubleFloatStruct.valueOf(f1 / d);
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
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 / f2);
		}
	}

	/**
	 * {@link FloatStruct.FloatEqualToVisitor} for computing numeric '=' equality results for {@link
	 * SingleFloatStruct}s.
	 */
	private static final class SingleFloatEqualToVisitor extends FloatStruct.FloatEqualToVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatEqualToVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param number1
		 * 		the first argument in the numeric '=' equality operation
		 */
		private SingleFloatEqualToVisitor(final SingleFloatStruct number1) {
			super(number1);
		}

		@Override
		public boolean equalTo(final SingleFloatStruct number2) {
			return Float.compare(number1.f, number2.f) == 0;
		}

		@Override
		public boolean equalTo(final DoubleFloatStruct number2) {
			return Double.compare(number1.f, number2.d) == 0;
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
	 * SingleFloatStruct}s.
	 */
	private static final class SingleFloatLessThanVisitor extends FloatStruct.FloatLessThanVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatLessThanVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<'} equality operation
		 */
		private SingleFloatLessThanVisitor(final SingleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThan(final SingleFloatStruct real2) {
			return Float.compare(real1.f, real2.f) < 0;
		}

		@Override
		public boolean lessThan(final DoubleFloatStruct real2) {
			return Double.compare(real1.f, real2.d) < 0;
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
	 * SingleFloatStruct}s.
	 */
	private static final class SingleFloatGreaterThanVisitor extends FloatStruct.FloatGreaterThanVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatGreaterThanVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>'} equality operation
		 */
		private SingleFloatGreaterThanVisitor(final SingleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThan(final SingleFloatStruct real2) {
			return Float.compare(real1.f, real2.f) > 0;
		}

		@Override
		public boolean greaterThan(final DoubleFloatStruct real2) {
			return Double.compare(real1.f, real2.d) > 0;
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
	 * {@link SingleFloatStruct}s.
	 */
	private static final class SingleFloatLessThanOrEqualToVisitor extends FloatStruct.FloatLessThanOrEqualToVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatLessThanOrEqualToVisitor with the provided
		 * {@link SingleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '<='} equality operation
		 */
		private SingleFloatLessThanOrEqualToVisitor(final SingleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean lessThanOrEqualTo(final SingleFloatStruct real2) {
			return Float.compare(real1.f, real2.f) <= 0;
		}

		@Override
		public boolean lessThanOrEqualTo(final DoubleFloatStruct real2) {
			return Double.compare(real1.f, real2.d) <= 0;
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
	 * {@link SingleFloatStruct}s.
	 */
	private static final class SingleFloatGreaterThanOrEqualToVisitor extends FloatStruct.FloatGreaterThanOrEqualToVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatGreaterThanOrEqualToVisitor with the provided
		 * {@link SingleFloatStruct}.
		 *
		 * @param real1
		 * 		the first argument in the numeric {@literal '>='} equality operation
		 */
		private SingleFloatGreaterThanOrEqualToVisitor(final SingleFloatStruct real1) {
			super(real1);
		}

		@Override
		public boolean greaterThanOrEqualTo(final SingleFloatStruct real2) {
			return Float.compare(real1.f, real2.f) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final DoubleFloatStruct real2) {
			return Double.compare(real1.f, real2.d) >= 0;
		}

		@Override
		public boolean greaterThanOrEqualTo(final BigFloatStruct real2) {
			final BigDecimal bigDecimal1 = real1.bigDecimalValue();
			final BigDecimal bigDecimal2 = real2.bigDecimal;
			return bigDecimal1.compareTo(bigDecimal2) >= 0;
		}
	}
}
