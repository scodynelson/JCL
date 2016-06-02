/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.math.BigDecimal;

import jcl.types.FloatType;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apfloat.Apfloat;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link FloatStruct2} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct2 extends RealStruct2Impl<Apfloat> {

	/**
	 * {@link FloatStruct2} constant representing 0.0.
	 */
	public static final FloatStruct2 ZERO = valueOf(0.0D);

	/**
	 * {@link FloatStruct2} constant representing -0.0.
	 */
	public static final FloatStruct2 MINUS_ZERO = valueOf(-0.0D);

	/**
	 * {@link FloatStruct2} constant representing 1.0.
	 */
	public static final FloatStruct2 ONE = valueOf(1.0D);

	/**
	 * {@link FloatStruct2} constant representing -1.0.
	 */
	public static final FloatStruct2 MINUS_ONE = valueOf(-1.0D);

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(FloatStruct2.class);

	/**
	 * The floating-point precision of a DoubleFloatStruct object.
	 */
	private static final int DOUBLE_PRECISION = 53;

	private final Apfloat apfloat;

	/**
	 * Private constructor.
	 *
	 * @param apfloat
	 * 		the value of the BigIntegerStruct
	 */
	private FloatStruct2(final Apfloat apfloat) {
		super(FloatType.INSTANCE, null);
		this.apfloat = apfloat;
	}

	/**
	 * Returns a DoubleFloatStruct object with the provided {@code double} value.
	 *
	 * @param f
	 * 		the {@code double} value of the resulting DoubleFloatStruct
	 *
	 * @return a DoubleFloatStruct object with the provided {@code double} value
	 */
	public static FloatStruct2 valueOf(final float f) {
		final Apfloat apfloat = new Apfloat(f);
		return valueOf(apfloat);
	}

	/**
	 * Returns a DoubleFloatStruct object with the provided {@code double} value.
	 *
	 * @param d
	 * 		the {@code double} value of the resulting DoubleFloatStruct
	 *
	 * @return a DoubleFloatStruct object with the provided {@code double} value
	 */
	public static FloatStruct2 valueOf(final double d) {
		final Apfloat apfloat = new Apfloat(d);
		return valueOf(apfloat);
	}

	/**
	 * Returns a DoubleFloatStruct object with the provided {@code double} value.
	 *
	 * @param bigDecimal
	 * 		the {@code double} value of the resulting DoubleFloatStruct
	 *
	 * @return a DoubleFloatStruct object with the provided {@code double} value
	 */
	public static FloatStruct2 valueOf(final BigDecimal bigDecimal) {
		final Apfloat apfloat = new Apfloat(bigDecimal);
		return valueOf(apfloat);
	}

	/**
	 * Returns a BigIntegerStruct object with the provided {@link Apfloat} value.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} value of the resulting BigIntegerStruct
	 *
	 * @return a BigIntegerStruct object with the provided {@link Apfloat} value
	 */
	public static FloatStruct2 valueOf(final Apfloat apfloat) {
		return new FloatStruct2(apfloat);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link String}. This will subclass appropriately to the
	 * IntegerStruct implementation that would most accurately hold the integer data structure.
	 *
	 * @param s
	 * 		the {@link String} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link String}
	 */
	public static FloatStruct2 valueOf(final String s) {
		final Apfloat apfloat = new Apfloat(s);
		return valueOf(apfloat);
	}

	/**
	 * Returns a BigIntegerStruct object with the provided {@link Apfloat} value.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} value of the resulting BigIntegerStruct
	 *
	 * @return a BigIntegerStruct object with the provided {@link Apfloat} value
	 */
	public static FloatStruct2 valueOf(final Apfloat apfloat, final FloatStruct2 prototype) {
		return new FloatStruct2(apfloat);
	}

	public Apfloat getAp() {
		return apfloat;
	}

	/**
	 * Returns this FloatStruct as a {@code float} value.
	 *
	 * @return this FloatStruct as a {@code float} value
	 */
	public float floatValue() {
		return apfloat.floatValue();
	}

	/**
	 * Returns this FloatStruct as a {@code double} value.
	 *
	 * @return this FloatStruct as a {@code double} value
	 */
	public double doubleValue() {
		return apfloat.doubleValue();
	}

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign..
	 *
	 * @return a {@link DecodeFloatResult2} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	public DecodeFloatResult2 decodeFloat() {
		// TODO
		final double d = apfloat.doubleValue();
		// TODO
		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final int expt = ArithmeticUtils.pow(2, DOUBLE_PRECISION);
		final long significand = mantissa / expt;
		final FloatStruct2 significandFloat = valueOf(significand);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = (storedExponent - 1075) + DOUBLE_PRECISION;
		final IntegerStruct2 exponentInteger = IntegerStruct2.valueOf(exponent);

		final int sign = decodedDouble.getSign();
		final FloatStruct2 signFloat = (sign == 1) ? ONE : MINUS_ONE;

		return new DecodeFloatResult2(significandFloat, exponentInteger, signFloat);
	}

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * difference between this method an {@link #decodeFloat()} is that the significand and sign will both be {@link
	 * IntegerStruct2}s with a special weighting between the significand and exponent based on the scaling needed for
	 * the
	 * significand to produce an {@link IntegerStruct2}.
	 *
	 * @return a {@link DecodeFloatResult2} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	public DecodeFloatResult2 integerDecodeFloat() {
		// TODO
		final double d = apfloat.doubleValue();
		// TODO
		final long bits = Double.doubleToRawLongBits(d);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final IntegerStruct2 significandInteger = IntegerStruct2.valueOf(mantissa);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = storedExponent - 1075;
		final IntegerStruct2 exponentInteger = IntegerStruct2.valueOf(exponent);

		final int sign = decodedDouble.getSign();
		final IntegerStruct2 signInteger = (sign == 1) ? IntegerStruct2.ONE : IntegerStruct2.MINUS_ONE;

		return new DecodeFloatResult2(significandInteger, exponentInteger, signInteger);
	}

	/**
	 * Returns (* float (expt (float b float) scale)), where b is the radix of the floating-point representation.
	 *
	 * @param scale
	 * 		the
	 *
	 * @return this FloatStruct scaled to the provided scale value
	 */
	public NumberStruct2 scaleFloat(final IntegerStruct2 scale) {
		final IntegerStruct2 radix = floatRadix();
		final NumberStruct2 expt = radix.expt(scale);
		return multiply(expt);
	}

	/**
	 * Returns the number of radix b digits used in the representation of this FloatStruct.
	 *
	 * @return the number of radix b digits used in the representation of this FloatStruct
	 */
	public IntegerStruct2 floatDigits() {
		return floatPrecision();
	}

	/**
	 * Returns the number of significant radix b digits present in this FloatStruct.
	 *
	 * @return the number of significant radix b digits present in this FloatStruct
	 */
	public IntegerStruct2 floatPrecision() {
		return IntegerStruct2.valueOf(DOUBLE_PRECISION);
	}

	/**
	 * The radix of the FloatStruct.
	 *
	 * @return the radix of the FloatStruct
	 */
	public IntegerStruct2 floatRadix() {
		return IntegerStruct2.TWO;
	}

	/**
	 * Returns either a {@code 1} or a {@code -1} value based on the sign of the FloatStruct.
	 *
	 * @return a {@code 1} or a {@code -1} value based on the sign of the FloatStruct
	 */
	public FloatStruct2 floatSign() {
		// TODO
		final double d = apfloat.doubleValue();
		// TODO
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
	public FloatStruct2 floatSign(final FloatStruct2 float2) {
		if (minusp()) {
			if (float2.minusp()) {
				return float2;
			} else {
				return (FloatStruct2) float2.negation();
			}
		} else {
			return (FloatStruct2) float2.abs();
		}
	}

	/**
	 * Decodes the double by the provided {@code long} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 *
	 * @param bits
	 * 		the {@code long} bits representing the {@code double} value
	 *
	 * @return the {@link FloatStruct2.DecodedDouble} wrapping the decoded sign, exponent, and mantissa values
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

	@Override
	public RealStruct2 abs() {
		return null;
	}

	@Override
	public boolean zerop() {
		return false;
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		return false;
	}

	@Override
	public NumberStruct2 signum() {
		return null;
	}

	@Override
	public RealStruct2 imagPart() {
		return null;
	}

	@Override
	public RealStruct2 negation() {
		return null;
	}

	@Override
	public RealStruct2 reciprocal() {
		return null;
	}

	@Override
	public RealStruct2 exp() {
		return null;
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
		return null;
	}

	@Override
	public RealStruct2 log() {
		return null;
	}

	@Override
	public RealStruct2 sqrt() {
		return null;
	}

	@Override
	public RealStruct2 sin() {
		return null;
	}

	@Override
	public RealStruct2 cos() {
		return null;
	}

	@Override
	public RealStruct2 tan() {
		return null;
	}

	@Override
	public RealStruct2 asin() {
		return null;
	}

	@Override
	public RealStruct2 acos() {
		return null;
	}

	@Override
	public RealStruct2 atan() {
		return null;
	}

	@Override
	public RealStruct2 atan(final RealStruct2 real) {
		return null;
	}

	@Override
	public RealStruct2 sinh() {
		return null;
	}

	@Override
	public RealStruct2 cosh() {
		return null;
	}

	@Override
	public RealStruct2 tanh() {
		return null;
	}

	@Override
	public RealStruct2 asinh() {
		return null;
	}

	@Override
	public RealStruct2 acosh() {
		return null;
	}

	@Override
	public RealStruct2 atanh() {
		return null;
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
	public boolean isLessThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean plusp() {
		return false;
	}

	@Override
	public boolean minusp() {
		return false;
	}

	@Override
	public RationalStruct2 rational() {
		return null;
	}

	@Override
	public FloatStruct2 floatingPoint() {
		return this;
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return null;
	}

	@Override
	public RealStruct2 mod(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public RealStruct2 rem(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 floor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 ffloor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 ceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 fceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 truncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 ftruncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 round(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 fround(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public Apfloat ap() {
		return ap;
	}
}
