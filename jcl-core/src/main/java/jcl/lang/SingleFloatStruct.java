package jcl.lang;

import jcl.lang.internal.SingleFloatStructImpl;

public interface SingleFloatStruct extends FloatStruct {

	/**
	 * {@link SingleFloatStruct} constant representing 0.0.
	 */
	SingleFloatStruct ZERO = toLispFloat(0.0F);

	/**
	 * {@link SingleFloatStruct} constant representing -0.0.
	 */
	SingleFloatStruct MINUS_ZERO = toLispFloat(-0.0F);

	/**
	 * {@link SingleFloatStruct} constant representing 1.0.
	 */
	SingleFloatStruct ONE = toLispFloat(1.0F);

	/**
	 * {@link SingleFloatStruct} constant representing -1.0.
	 */
	SingleFloatStruct MINUS_ONE = toLispFloat(-1.0F);

	static SingleFloatStruct toLispFloat(final float value) {
		return new SingleFloatStructImpl(value);
	}

	static SingleFloatStruct toLispFloat(final Float value) {
		return new SingleFloatStructImpl(value);
	}

	static SingleFloatStruct toLispFloat(final double value) {
		return new SingleFloatStructImpl(value);
	}

	static SingleFloatStruct toLispFloat(final Double value) {
		return new SingleFloatStructImpl(value);
	}

	@Override
	default FloatStruct imagPart() {
		return ZERO;
	}
}
