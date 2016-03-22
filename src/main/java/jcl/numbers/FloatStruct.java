/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;

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

	/**
	 * Getter for float {@link #bigDecimal} property.
	 *
	 * @return float {@link #bigDecimal} property
	 */
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
