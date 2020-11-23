package jcl.lang;

import jcl.lang.internal.SingleFloatStructImpl;

/**
 * The {@link SingleFloatStruct} is the object representation of a Lisp 'single-float' type.
 */
public interface SingleFloatStruct extends FloatStruct {

	/**
	 * {@link SingleFloatStruct} constant representing 0.0.
	 */
	SingleFloatStruct ZERO = new SingleFloatStructImpl(0.0F);

	/**
	 * {@link SingleFloatStruct} constant representing -0.0.
	 */
	SingleFloatStruct MINUS_ZERO = new SingleFloatStructImpl(-0.0F);

	/**
	 * {@link SingleFloatStruct} constant representing 1.0.
	 */
	SingleFloatStruct ONE = new SingleFloatStructImpl(1.0F);

	/**
	 * {@link SingleFloatStruct} constant representing -1.0.
	 */
	SingleFloatStruct MINUS_ONE = new SingleFloatStructImpl(-1.0F);

	/**
	 * Returns a new SingleFloatStruct representation of the provided {@literal float}.
	 *
	 * @param value
	 * 		the {@literal float} to represent as a SingleFloatStruct
	 *
	 * @return a new SingleFloatStruct representation of the provided {@literal float}
	 */
	static SingleFloatStruct toLispFloat(final float value) {
		if (Float.compare(value, 0.0F) == 0) {
			return ZERO;
		} else if (Float.compare(value, -0.0F) == 0) {
			return MINUS_ZERO;
		} else if (Float.compare(value, 1.0F) == 0) {
			return ONE;
		} else if (Float.compare(value, -1.0F) == 0) {
			return MINUS_ONE;
		} else {
			return new SingleFloatStructImpl(value);
		}
	}

	/**
	 * Returns a new SingleFloatStruct representation of the provided {@link Float}.
	 *
	 * @param value
	 * 		the {@link Float} to represent as a SingleFloatStruct
	 *
	 * @return a new SingleFloatStruct representation of the provided {@link Float}
	 */
	static SingleFloatStruct toLispFloat(final Float value) {
		return toLispFloat(value.floatValue());
	}

	/**
	 * Returns a new SingleFloatStruct representation of the provided {@literal double}.
	 *
	 * @param value
	 * 		the {@literal double} to represent as a SingleFloatStruct
	 *
	 * @return a new SingleFloatStruct representation of the provided {@literal double}
	 */
	@SuppressWarnings("NumericCastThatLosesPrecision")
	static SingleFloatStruct toLispFloat(final double value) {
		return toLispFloat((float) value);
	}

	/**
	 * Returns a new SingleFloatStruct representation of the provided {@link Double}.
	 *
	 * @param value
	 * 		the {@link Double} to represent as a SingleFloatStruct
	 *
	 * @return a new SingleFloatStruct representation of the provided {@link Double}
	 */
	static SingleFloatStruct toLispFloat(final Double value) {
		return toLispFloat(value.floatValue());
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	default FloatStruct imagPart() {
		return ZERO;
	}
}
