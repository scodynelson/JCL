/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;

import org.apfloat.Apfloat;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public interface FloatStruct extends RealStruct {

	/**
	 * {@link FloatStruct} constant representing 0.0.
	 */
	FloatStruct ZERO = SingleFloatStruct.ZERO;

	/**
	 * {@link FloatStruct} constant representing -0.0.
	 */
	FloatStruct MINUS_ZERO = SingleFloatStruct.MINUS_ZERO;

	/**
	 * {@link FloatStruct} constant representing 1.0.
	 */
	FloatStruct ONE = SingleFloatStruct.ONE;

	/**
	 * {@link FloatStruct} constant representing -1.0.
	 */
	FloatStruct MINUS_ONE = SingleFloatStruct.MINUS_ONE;

	static boolean canDoubleBeFloat(final Double d) {
		final Float f = new Float(d);
		if (f.isInfinite()) {
			return false;
		}
		final BigDecimal floatBigDecimal = new BigDecimal(String.valueOf(f));
		final BigDecimal doubleBigDecimal = new BigDecimal(String.valueOf(d));
		return doubleBigDecimal.compareTo(floatBigDecimal) == 0;
	}

	static boolean canBigDecimalBeDouble(final BigDecimal bigDecimal) {
		final double d = bigDecimal.doubleValue();
		final BigDecimal doubleBigDecimal = new BigDecimal(String.valueOf(d));
		return doubleBigDecimal.compareTo(bigDecimal) == 0;
	}

	static boolean canBigDecimalBeFloat(final BigDecimal bigDecimal) {
		return canBigDecimalBeDouble(bigDecimal) && canDoubleBeFloat(bigDecimal.doubleValue());
	}

	@Deprecated
	static FloatStruct valueOf(final Apfloat apfloat) {
		return SingleFloatStruct.valueOf(apfloat.floatValue());
	}

	static FloatStruct valueOf(final Float f) {
		return SingleFloatStruct.valueOf(f);
	}

	static FloatStruct valueOf(final Double d) {
		if (canDoubleBeFloat(d)) {
			final float f = new Float(d);
			return SingleFloatStruct.valueOf(f);
		}
		return DoubleFloatStruct.valueOf(d);
	}

	static FloatStruct valueOf(final BigDecimal bigDecimal) {
		if (canBigDecimalBeFloat(bigDecimal)) {
			final float f = bigDecimal.floatValue();
			return valueOf(f);
		}
		if (canBigDecimalBeFloat(bigDecimal)) {
			final double d = bigDecimal.doubleValue();
			return valueOf(d);
		}
		return BigFloatStruct.valueOf(bigDecimal);
	}

	static FloatStruct valueOf(final String s) {
		try {
			final float f = Float.parseFloat(s);
			return valueOf(f);
		} catch (final NumberFormatException ignore) {
		}
		try {
			final double d = Double.parseDouble(s);
			return valueOf(d);
		} catch (final NumberFormatException ignore) {
		}
		final BigDecimal bigDecimal = new BigDecimal(s);
		return valueOf(bigDecimal);
	}

	float floatValue();

	double doubleValue();

	BigDecimal bigDecimalValue();

	@Deprecated
	BigDecimal getBigDecimal();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * calculation for these values are based on the decoding for Java {@link Double} values from the algorithm defined
	 * in {@link Double#longBitsToDouble}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult decodeFloat();

	default NumberStruct scaleFloat(final IntegerStruct scale) {
		final IntegerStruct radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	default IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	default FloatStruct floatSign() {
		return floatSign(ONE);
	}

	FloatStruct floatSign(final FloatStruct float2);

	default IntegerStruct floatDigits() {
		return floatPrecision();
	}

	IntegerStruct floatPrecision();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * calculation for these values are based on the decoding for Java {@link Double} values from the algorithm defined
	 * in {@link Double#longBitsToDouble}. The difference between this method an {@link #decodeFloat()} is that the
	 * significand and sign will both be {@link IntegerStruct}s with a special weighting between the significand and
	 * exponent based on the scaling needed for the significand to produce an {@link IntegerStruct}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult integerDecodeFloat();
}
