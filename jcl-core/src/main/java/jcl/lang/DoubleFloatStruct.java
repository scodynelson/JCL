package jcl.lang;

import jcl.lang.internal.DoubleFloatStructImpl;

/**
 * The {@link DoubleFloatStruct} is the object representation of a Lisp 'double-float' type.
 */
public interface DoubleFloatStruct extends FloatStruct {

	/**
	 * {@link DoubleFloatStruct} constant representing 0.0.
	 */
	DoubleFloatStruct ZERO = new DoubleFloatStructImpl(0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -0.0.
	 */
	DoubleFloatStruct MINUS_ZERO = new DoubleFloatStructImpl(-0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing 1.0.
	 */
	DoubleFloatStruct ONE = new DoubleFloatStructImpl(1.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -1.0.
	 */
	DoubleFloatStruct MINUS_ONE = new DoubleFloatStructImpl(-1.0D);

	/**
	 * Returns a new DoubleFloatStruct representation of the provided {@literal float}.
	 *
	 * @param value
	 * 		the {@literal float} to represent as a DoubleFloatStruct
	 *
	 * @return a new DoubleFloatStruct representation of the provided {@literal float}
	 */
	static DoubleFloatStruct toLispFloat(final float value) {
		return toLispFloat((double) value);
	}

	/**
	 * Returns a new DoubleFloatStruct representation of the provided {@link Float}.
	 *
	 * @param value
	 * 		the {@link Float} to represent as a DoubleFloatStruct
	 *
	 * @return a new DoubleFloatStruct representation of the provided {@link Float}
	 */
	static DoubleFloatStruct toLispFloat(final Float value) {
		return toLispFloat(value.doubleValue());
	}

	/**
	 * Returns a new DoubleFloatStruct representation of the provided {@literal double}.
	 *
	 * @param value
	 * 		the {@literal double} to represent as a DoubleFloatStruct
	 *
	 * @return a new DoubleFloatStruct representation of the provided {@literal double}
	 */
	static DoubleFloatStruct toLispFloat(final double value) {
		if (Double.compare(value, 0.0D) == 0) {
			return ZERO;
		} else if (Double.compare(value, -0.0D) == 0) {
			return MINUS_ZERO;
		} else if (Double.compare(value, 1.0D) == 0) {
			return ONE;
		} else if (Double.compare(value, -1.0D) == 0) {
			return MINUS_ONE;
		} else {
			return new DoubleFloatStructImpl(value);
		}
	}

	/**
	 * Returns a new DoubleFloatStruct representation of the provided {@link Double}.
	 *
	 * @param value
	 * 		the {@link Double} to represent as a DoubleFloatStruct
	 *
	 * @return a new DoubleFloatStruct representation of the provided {@link Double}
	 */
	static DoubleFloatStruct toLispFloat(final Double value) {
		return toLispFloat(value.doubleValue());
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	default FloatStruct imagPart() {
		return ZERO;
	}
}
