package jcl.lang;

import java.math.BigDecimal;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public interface FloatStruct extends RealStruct {

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign..
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult decodeFloat();

	/**
	 * Computes the three main values that characterize this FloatStruct: the significand, exponent, and sign. The
	 * difference between this method an {@link #decodeFloat()} is that the significand and sign will both be {@link
	 * IntegerStruct}s with a special weighting between the significand and exponent based on the scaling needed for
	 * the significand to produce an {@link IntegerStruct}.
	 *
	 * @return a {@link DecodeFloatResult} containing the decoded significand, exponent, and sign for this FloatStruct
	 */
	DecodeFloatResult integerDecodeFloat();

	/**
	 * Returns (* float (expt (float b float) scale)), where b is the radix of the floating-point representation.
	 *
	 * @param scale
	 * 		the
	 *
	 * @return this FloatStruct scaled to the provided scale value
	 */
	default NumberStruct scaleFloat(final IntegerStruct scale) {
		final IntegerStruct radix = floatRadix();
		final NumberStruct expt = radix.expt(scale);
		return multiply(expt);
	}

	/**
	 * Returns the number of radix b digits used in the representation of this FloatStruct.
	 *
	 * @return the number of radix b digits used in the representation of this FloatStruct
	 */
	default IntegerStruct floatDigits() {
		return floatPrecision();
	}

	/**
	 * Returns the number of significant radix b digits present in this FloatStruct.
	 *
	 * @return the number of significant radix b digits present in this FloatStruct
	 */
	IntegerStruct floatPrecision();

	/**
	 * The radix of the FloatStruct.
	 *
	 * @return the radix of the FloatStruct
	 */
	default IntegerStruct floatRadix() {
		return IntegerStruct.TWO;
	}

	/**
	 * Returns either a {@code 1} or a {@code -1} value based on the sign of the FloatStruct.
	 *
	 * @return a {@code 1} or a {@code -1} value based on the sign of the FloatStruct
	 */
	FloatStruct floatSign();

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
	default FloatStruct floatSign(final FloatStruct float2) {
		if (minusp().toJavaPBoolean()) {
			if (float2.minusp().toJavaPBoolean()) {
				return float2;
			} else {
				return float2.negation();
			}
		} else {
			return float2.abs();
		}
	}

	/**
	 * Returns the {@literal float} representation of the FloatStruct.
	 *
	 * @return a {@literal float} representation of the FloatStruct
	 */
	float toJavaPFloat();

	/**
	 * Returns the {@link Float} representation of the FloatStruct.
	 *
	 * @return a {@link Float} representation of the FloatStruct
	 */
	Float toJavaFloat();

	/**
	 * Returns the {@literal double} representation of the FloatStruct.
	 *
	 * @return a {@literal double} representation of the FloatStruct
	 */
	double toJavaPDouble();

	/**
	 * Returns the {@link Double} representation of the FloatStruct.
	 *
	 * @return a {@link Double} representation of the FloatStruct
	 */
	Double toJavaDouble();

	/**
	 * Returns the {@link BigDecimal} representation of the FloatStruct.
	 *
	 * @return a {@link BigDecimal} representation of the FloatStruct
	 */
	BigDecimal toJavaBigDecimal();

	/*
	REAL-STRUCT
	 */

	@Override
	default FloatStruct floatingPoint() {
		return this;
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	FloatStruct abs();

	@Override
	FloatStruct signum();

	@Override
	default FloatStruct realPart() {
		return this;
	}

	@Override
	default FloatStruct conjugate() {
		return this;
	}

	@Override
	FloatStruct negation();

	@Override
	FloatStruct reciprocal();

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof FloatStruct)
						&& ((FloatStruct) object).ap().equals(ap()));
	}
}
