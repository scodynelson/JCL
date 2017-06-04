package jcl.lang;

import jcl.lang.internal.SingleFloatStructImpl;

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

	static SingleFloatStruct toLispFloat(final Float value) {
		return toLispFloat(value.floatValue());
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	static SingleFloatStruct toLispFloat(final double value) {
		return toLispFloat((float) value);
	}

	static SingleFloatStruct toLispFloat(final Double value) {
		return toLispFloat(value.floatValue());
	}

	@Override
	default FloatStruct imagPart() {
		return ZERO;
	}
}
