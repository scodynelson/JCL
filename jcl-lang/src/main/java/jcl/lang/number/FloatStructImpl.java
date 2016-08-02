/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.number;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.type.DoubleFloatType;
import jcl.type.FloatType;
import jcl.type.LongFloatType;
import jcl.type.ShortFloatType;
import jcl.type.SingleFloatType;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * The {@link FloatStructImpl} is the object representation of a Lisp 'float' type.
 */
public final class FloatStructImpl extends RealStructImpl<Apfloat> {

	/**
	 * {@link FloatStructImpl} constant representing 0.0.
	 */
	public static final FloatStructImpl ZERO = valueOf(0.0D);

	/**
	 * {@link FloatStructImpl} constant representing -0.0.
	 */
	public static final FloatStructImpl MINUS_ZERO = valueOf(-0.0D);

	/**
	 * {@link FloatStructImpl} constant representing 1.0.
	 */
	public static final FloatStructImpl ONE = valueOf(1.0D);

	/**
	 * {@link FloatStructImpl} constant representing -1.0.
	 */
	public static final FloatStructImpl MINUS_ONE = valueOf(-1.0D);

	/**
	 * The floating-point precision of a FloatStruct object.
	 */
	private static final int DOUBLE_PRECISION = 53;

	/**
	 * Private constructor.
	 *
	 * @param apfloat
	 * 		the value of the FloatStruct
	 */
	private FloatStructImpl(final Apfloat apfloat) {
		super(FloatType.INSTANCE, apfloat);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Float} value.
	 *
	 * @param f
	 * 		the {@link Float} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link Float} value
	 */
	public static FloatStructImpl valueOf(final Float f) {
		final Apfloat apfloat = new Apfloat(f);
		return valueOf(apfloat);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Double} value.
	 *
	 * @param d
	 * 		the {@link Double} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link Double} value
	 */
	public static FloatStructImpl valueOf(final Double d) {
		final Apfloat apfloat = new Apfloat(d);
		return valueOf(apfloat);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link BigDecimal} value.
	 *
	 * @param bigDecimal
	 * 		the {@link BigDecimal} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link BigDecimal} value
	 */
	public static FloatStructImpl valueOf(final BigDecimal bigDecimal) {
		final Apfloat apfloat = new Apfloat(bigDecimal);
		return valueOf(apfloat);
	}

	/**
	 * Returns a new FloatStruct representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new FloatStruct
	 *
	 * @return a new FloatStruct representing the provided {@link String}
	 */
	public static FloatStructImpl valueOf(final String s) {
		final Apfloat apfloat = new Apfloat(s);
		return valueOf(apfloat);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Apfloat} value.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link Apfloat} value
	 */
	public static FloatStructImpl valueOf(final Apfloat apfloat) {
		return new FloatStructImpl(apfloat);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Apfloat} value.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} value of the resulting FloatStruct
	 * @param prototype
	 * 		the FloatStruct to use as a prototype for the resulting floating point precision
	 * 		precision
	 *
	 * @return a FloatStruct object with the provided {@link Apfloat} value
	 */
	public static FloatStructImpl valueOf(final Apfloat apfloat, final FloatStructImpl prototype) {
		final long precision = prototype.ap.precision();
		final Apfloat preciseApfloat = apfloat.precision(precision);
		return valueOf(preciseApfloat);
	}

	/**
	 * Returns this FloatStruct as a {@code float} value.
	 *
	 * @return this FloatStruct as a {@code float} value
	 */
	public float floatValue() {
		return ap.floatValue();
	}

	/**
	 * Returns this FloatStruct as a {@code double} value.
	 *
	 * @return this FloatStruct as a {@code double} value
	 */
	public double doubleValue() {
		return ap.doubleValue();
	}

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign..
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	public DecodeFloatResult decodeFloat() {
		final double d = ap.doubleValue();

		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final int expt = ArithmeticUtils.pow(2, DOUBLE_PRECISION);
		final long significand = mantissa / expt;
		final FloatStructImpl significandFloat = valueOf(Double.valueOf(significand));

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = (storedExponent - 1075) + DOUBLE_PRECISION;
		final IntegerStructImpl exponentInteger = IntegerStructImpl.valueOf(exponent);

		final int sign = decodedDouble.getSign();
		final FloatStructImpl signFloat = (sign == 1) ? ONE : MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * difference between this method an {@link #decodeFloat()} is that the significand and sign will both be {@link
	 * IntegerStructImpl}s with a special weighting between the significand and exponent based on the scaling needed for
	 * the significand to produce an {@link IntegerStructImpl}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	public DecodeFloatResult integerDecodeFloat() {
		final double d = ap.doubleValue();

		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final IntegerStructImpl significandInteger = IntegerStructImpl.valueOf(mantissa);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = storedExponent - 1075;
		final IntegerStructImpl exponentInteger = IntegerStructImpl.valueOf(exponent);

		final int sign = decodedDouble.getSign();
		final IntegerStructImpl signInteger = (sign == 1) ? IntegerStructImpl.ONE : IntegerStructImpl.MINUS_ONE;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	/**
	 * Returns (* float (expt (float b float) scale)), where b is the radix of the floating-point representation.
	 *
	 * @param scale
	 * 		the
	 *
	 * @return this FloatStruct scaled to the provided scale value
	 */
	public NumberStruct scaleFloat(final IntegerStructImpl scale) {
		final IntegerStructImpl radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	/**
	 * Returns the number of radix b digits used in the representation of this FloatStruct.
	 *
	 * @return the number of radix b digits used in the representation of this FloatStruct
	 */
	public IntegerStructImpl floatDigits() {
		return floatPrecision();
	}

	/**
	 * Returns the number of significant radix b digits present in this FloatStruct.
	 *
	 * @return the number of significant radix b digits present in this FloatStruct
	 */
	public IntegerStructImpl floatPrecision() {
		return IntegerStructImpl.valueOf(DOUBLE_PRECISION);
	}

	/**
	 * The radix of the FloatStruct.
	 *
	 * @return the radix of the FloatStruct
	 */
	public IntegerStructImpl floatRadix() {
		return IntegerStructImpl.TWO;
	}

	/**
	 * Returns either a {@code 1} or a {@code -1} value based on the sign of the FloatStruct.
	 *
	 * @return a {@code 1} or a {@code -1} value based on the sign of the FloatStruct
	 */
	public FloatStructImpl floatSign() {
		final double d = ap.doubleValue();

		final long bits = Double.doubleToRawLongBits(d);
		return (bits < 0) ? MINUS_ONE : ONE;
	}

	/**
	 * Returns a number z such that z and this FloatStruct have the same sign and also such that z and float2 have the
	 * same absolute value.
	 *
	 * @param float2
	 * 		the value of the resulting FloatStruct
	 *
	 * @return a number z such that z and this FloatStruct have the same sign and also such that z and float2 have the
	 * same absolute value
	 */
	public FloatStructImpl floatSign(final FloatStructImpl float2) {
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				return float2.negation();
			}
		} else {
			return float2.abs();
		}
	}

	/**
	 * Decodes the double by the provided {@code long} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 *
	 * @param bits
	 * 		the {@code long} bits representing the {@code double} value
	 *
	 * @return the {@link FloatStructImpl.DecodedDouble} wrapping the decoded sign, exponent, and mantissa values
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
	public RationalStruct rational() {
		final double d = ap.doubleValue();
		final BigFraction bigFraction = new BigFraction(d);
		final BigFraction bigFractionReduced = bigFraction.reduce();

		final BigInteger numerator = bigFractionReduced.getNumerator();
		final BigInteger denominator = bigFractionReduced.getDenominator();

		if (BigInteger.ONE.equals(denominator)) {
			return IntegerStructImpl.valueOf(numerator);
		}

		final Apint numeratorAp = new Apint(numerator);
		final Apint denominatorAp = new Apint(denominator);
		return RatioStructImpl.valueOf(numeratorAp, denominatorAp);
	}

	@Override
	public FloatStructImpl floatingPoint() {
		return this;
	}

	@Override
	public FloatStructImpl floatingPoint(final FloatStructImpl prototype) {
		return valueOf(ap, prototype);
	}

	/*
		NumberStruct
	 */

	@Override
	public FloatStructImpl abs() {
		final Apfloat abs = ApfloatMath.abs(ap);
		return valueOf(abs);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat add = ap.add((Apfloat) numberAp);
			return valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat subtract = ap.subtract((Apfloat) numberAp);
			return valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat multiply = ap.multiply((Apfloat) numberAp);
			return valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat divide = ap.divide((Apfloat) numberAp);
			return valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public FloatStructImpl signum() {
		final int signum = ap.signum();
		if (signum == 0) {
			return ZERO;
		}
		if (signum > 0) {
			return ONE;
		}
		return MINUS_ONE;
	}

	@Override
	public FloatStructImpl realPart() {
		return this;
	}

	@Override
	public FloatStructImpl imagPart() {
		return ZERO;
	}

	@Override
	public FloatStructImpl conjugate() {
		return this;
	}

	@Override
	public FloatStructImpl negation() {
		final Apfloat negate = ap.negate();
		return valueOf(negate);
	}

	@Override
	public FloatStructImpl reciprocal() {
		final Apfloat reciprocal = Apcomplex.ONE.divide(ap);
		return valueOf(reciprocal);
	}

	/*
		ToString
	 */

	@Override
	public String toString() {

		final FloatType floatType = (FloatType) getType();
		final FloatType defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();

		String floatString = ap.toString(true);
		if (!floatType.equals(defaultFloatFormat)) {
			if (floatType.equals(ShortFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'S');
			} else if (floatType.equals(SingleFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'F');
			} else if (floatType.equals(DoubleFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'D');
			} else if (floatType.equals(LongFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'L');
			}
		}

		return floatString;
	}
}
