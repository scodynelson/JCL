package jcl.lang;

import jcl.lang.number.DecodeFloatResult;
import jcl.lang.number.FloatStructImpl;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public interface FloatStruct extends RealStruct {

	/**
	 * {@link FloatStruct} constant representing 0.0.
	 */
	FloatStruct ZERO = FloatStructImpl.valueOf(0.0D);

	/**
	 * {@link FloatStruct} constant representing -0.0.
	 */
	FloatStruct MINUS_ZERO = FloatStructImpl.valueOf(-0.0D);

	/**
	 * {@link FloatStruct} constant representing 1.0.
	 */
	FloatStruct ONE = FloatStructImpl.valueOf(1.0D);

	/**
	 * {@link FloatStruct} constant representing -1.0.
	 */
	FloatStruct MINUS_ONE = FloatStructImpl.valueOf(-1.0D);

	/**
	 * Returns this FloatStruct as a {@code float} value.
	 *
	 * @return this FloatStruct as a {@code float} value
	 */
	float floatValue();

	/**
	 * Returns this FloatStruct as a {@code double} value.
	 *
	 * @return this FloatStruct as a {@code double} value
	 */
	double doubleValue();

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

	/*
		RealStruct
	 */

	@Override
	default FloatStruct floatingPoint() {
		return this;
	}

	@Override
	FloatStruct floatingPoint(final FloatStruct prototype);

	/*
		NumberStruct
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
	default FloatStruct imagPart() {
		return ZERO;
	}

	@Override
	default FloatStruct conjugate() {
		return this;
	}

	@Override
	FloatStruct negation();

	@Override
	FloatStruct reciprocal();
}
