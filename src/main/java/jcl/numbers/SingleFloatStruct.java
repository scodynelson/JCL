/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

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

	/**
	 * Returns a SingleFloatStruct object with the provided {@code double} value converted to a {@code float}.
	 *
	 * @param d
	 * 		the {@code double} value of the resulting SingleFloatStruct
	 *
	 * @return a SingleFloatStruct object with the provided {@code double} value converted to a {@code float}
	 */
	public static SingleFloatStruct valueOf(final Double d) {
		return new SingleFloatStruct(NumberUtils.doubleToFloat(d));
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
	public RealStruct.FloatingPointVisitor<?> floatingPointVisitor() {
		return new SingleFloatFloatingPointVisitor(this);
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
		// 127 + 23 = 150
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
		// 127 + 23 = 150
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
		final int bits = Float.floatToRawIntBits(f);
		return (bits < 0) ? MINUS_ONE : ONE;
	}

	/**
	 * Decodes the float by the provided {@code int} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.4.
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
		return Float.compare(f, 0.0F) > 0;
	}

	@Override
	public boolean minusp() {
		return Float.compare(f, 0.0F) < 0;
	}

	@Override
	public RationalStruct rational() {
		final BigFraction bigFraction = new BigFraction(f);
		final BigFraction bigFractionReduced = bigFraction.reduce();
		return RationalStruct.valueOf(bigFractionReduced);
	}

	@Override
	public FloatStruct floatingPoint(final FloatingPointVisitor<?> floatingPointVisitor) {
		return floatingPointVisitor.floatingPoint(this);
	}

	@Override
	public QuotientRemainderResult floor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.floor(this);
	}

	@Override
	public QuotientRemainderResult ffloor(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ffloor(this);
	}

	@Override
	public QuotientRemainderResult ceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ceiling(this);
	}

	@Override
	public QuotientRemainderResult fceiling(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fceiling(this);
	}

	@Override
	public QuotientRemainderResult round(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.round(this);
	}

	@Override
	public QuotientRemainderResult fround(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.fround(this);
	}

	@Override
	public QuotientRemainderResult truncate(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.truncate(this);
	}

	@Override
	public QuotientRemainderResult ftruncate(final QuotientRemainderVisitor<?> quotientRemainderVisitor) {
		return quotientRemainderVisitor.ftruncate(this);
	}

	@Override
	public RealStruct.QuotientRemainderVisitor<?> quotientRemainderVisitor() {
		return new SingleFloatQuotientRemainderVisitor(this);
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
		return new SingleFloatExptVisitor(this);
	}

	@Override
	public boolean zerop() {
		return Float.compare(f, 0.0F) == 0;
	}

	@Override
	public RealStruct abs() {
		if (Float.compare(f, 0.0F) >= 0) {
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
		} else if (Float.compare(f, 0.0F) > 0) {
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

	@Override
	public String toString() {
		return String.valueOf(f);
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
		public RealStruct add(final RatioStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 + f2);
		}

		@Override
		public NumberStruct add(final ComplexStruct number2) {
			// TODO
			return super.add(number2);
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
		public RealStruct subtract(final RatioStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 - f2);
		}

		@Override
		public NumberStruct subtract(final ComplexStruct number2) {
			// TODO
			return super.subtract(number2);
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
		public RealStruct multiply(final RatioStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 * f2);
		}

		@Override
		public NumberStruct multiply(final ComplexStruct number2) {
			// TODO
			return super.multiply(number2);
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
		public RealStruct divide(final RatioStruct number2) {
			final float f1 = number1.f;
			final float f2 = number2.bigFraction.floatValue();
			return valueOf(f1 / f2);
		}

		@Override
		public NumberStruct divide(final ComplexStruct number2) {
			// TODO
			return super.divide(number2);
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
		public boolean equalTo(final ComplexStruct number2) {
			// TODO
			return super.equalTo(number2);
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
	}

	/**
	 * {@link FloatStruct.FloatQuotientRemainderVisitor} for computing quotient and remainder results for {@link
	 * SingleFloatStruct}s.
	 */
	private static final class SingleFloatQuotientRemainderVisitor extends FloatStruct.FloatQuotientRemainderVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatQuotientRemainderVisitor with the provided
		 * {@link SingleFloatStruct}.
		 *
		 * @param real
		 * 		the real argument in the computational quotient and remainder operation
		 */
		private SingleFloatQuotientRemainderVisitor(final SingleFloatStruct real) {
			super(real);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final SingleFloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			final BigDecimal realBD = NumberUtils.bigDecimalValue(real.f);
			final BigDecimal divisorBD = NumberUtils.bigDecimalValue(divisor.f);

			final BigDecimal quotient = realBD.divide(divisorBD, 0, roundingMode);
			final BigDecimal remainder = realBD.subtract(quotient.multiply(divisorBD));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = getFloatQuotient(real.f, divisor, quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			final FloatStruct remainderFloat = valueOf(remainder.doubleValue());
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		@Override
		public QuotientRemainderResult quotientRemainder(final DoubleFloatStruct divisor, final RoundingMode roundingMode,
		                                                 final boolean isQuotientFloat) {
			final BigDecimal realBD = NumberUtils.bigDecimalValue(real.f);
			final BigDecimal divisorBD = NumberUtils.bigDecimalValue(divisor.d);

			final BigDecimal quotient = realBD.divide(divisorBD, 0, roundingMode);
			final BigDecimal remainder = realBD.subtract(quotient.multiply(divisorBD));

			final RealStruct quotientReal;
			if (isQuotientFloat) {
				quotientReal = getFloatQuotient(real.f, divisor, quotient);
			} else {
				final BigInteger quotientBigInteger = quotient.toBigInteger();
				quotientReal = IntegerStruct.valueOf(quotientBigInteger);
			}

			final SingleFloatStruct remainderFloat = valueOf(remainder.doubleValue());
			return new QuotientRemainderResult(quotientReal, remainderFloat);
		}

		/**
		 * Retrieves the {@link SingleFloatStruct} quotient value, accounting for {@code 0.0} and {@code -0.0}
		 * appropriately.
		 *
		 * @param real
		 * 		the initial real value
		 * @param divisor
		 * 		the {@link FloatStruct} divisor
		 * @param quotient
		 * 		the {@link BigDecimal} quotient result
		 *
		 * @return the {@link SingleFloatStruct} quotient value
		 */
		private static SingleFloatStruct getFloatQuotient(final float real, final FloatStruct divisor,
		                                                  final BigDecimal quotient) {
			final SingleFloatStruct floatQuotient;
			if (BigDecimal.ZERO.compareTo(quotient) == 0) {
				if (Float.compare(real, 0.0F) < 0) {
					if (divisor.minusp()) {
						floatQuotient = ZERO;
					} else {
						floatQuotient = MINUS_ZERO;
					}
				} else if (divisor.minusp()) {
					floatQuotient = MINUS_ZERO;
				} else {
					floatQuotient = ZERO;
				}
			} else {
				floatQuotient = valueOf(quotient.doubleValue());
			}
			return floatQuotient;
		}
	}

	/**
	 * {@link RealStruct.RealExptVisitor} for computing exponential function results for {@link SingleFloatStruct}s.
	 */
	private static final class SingleFloatExptVisitor extends RealStruct.RealExptVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatExptVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param base
		 * 		the base argument in the exponential operation
		 */
		private SingleFloatExptVisitor(final SingleFloatStruct base) {
			super(base);
		}

		@Override
		public NumberStruct expt(final IntIntegerStruct power) {
			final float x = base.f;
			final float y = power.i;
			return exptSingleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final LongIntegerStruct power) {
			final float x = base.f;
			final float y = power.l;
			return exptSingleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final BigIntegerStruct power) {
			final float x = base.f;
			final float y = power.bigInteger.floatValue();
			return exptSingleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final SingleFloatStruct power) {
			final float x = base.f;
			final float y = power.f;
			return exptSingleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final DoubleFloatStruct power) {
			final double x = base.f;
			final double y = power.d;
			return exptDoubleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final RatioStruct power) {
			final float x = base.f;
			final float y = power.bigFraction.floatValue();
			return exptSingleFloat(x, y);
		}

		@Override
		public NumberStruct expt(final ComplexStruct power) {
			// TODO
			return super.expt(power);
		}
	}

	/**
	 * {@link RealStruct.FloatingPointVisitor} for converting a {@link RealStruct} to the {@link SingleFloatStruct}
	 * type of {@link #prototype}.
	 */
	private static final class SingleFloatFloatingPointVisitor extends RealStruct.FloatingPointVisitor<SingleFloatStruct> {

		/**
		 * Private constructor to make a new instance of an SingleFloatFloatingPointVisitor with the provided {@link
		 * SingleFloatStruct}.
		 *
		 * @param prototype
		 * 		the prototype argument in the floating-point conversion operation
		 */
		private SingleFloatFloatingPointVisitor(final SingleFloatStruct prototype) {
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
			return valueOf(real.bigInteger.floatValue());
		}

		@Override
		public FloatStruct floatingPoint(final SingleFloatStruct real) {
			return real;
		}

		@Override
		public FloatStruct floatingPoint(final DoubleFloatStruct real) {
			return valueOf(NumberUtils.doubleToFloat(real.d));
		}

		@Override
		public FloatStruct floatingPoint(final RatioStruct real) {
			return valueOf(real.bigFraction.floatValue());
		}
	}
}
