package jcl.lang;

import jcl.lang.internal.DoubleFloatStructImpl;

public interface DoubleFloatStruct extends FloatStruct {

	/**
	 * {@link DoubleFloatStruct} constant representing 0.0.
	 */
	DoubleFloatStruct ZERO = toLispFloat(0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -0.0.
	 */
	DoubleFloatStruct MINUS_ZERO = toLispFloat(-0.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing 1.0.
	 */
	DoubleFloatStruct ONE = toLispFloat(1.0D);

	/**
	 * {@link DoubleFloatStruct} constant representing -1.0.
	 */
	DoubleFloatStruct MINUS_ONE = toLispFloat(-1.0D);

	static DoubleFloatStruct toLispFloat(final float value) {
		return new DoubleFloatStructImpl(value);
	}

	static DoubleFloatStruct toLispFloat(final Float value) {
		return new DoubleFloatStructImpl(value);
	}

	static DoubleFloatStruct toLispFloat(final double value) {
		return new DoubleFloatStructImpl(value);
	}

	static DoubleFloatStruct toLispFloat(final Double value) {
		return new DoubleFloatStructImpl(value);
	}

	@Override
	default FloatStruct imagPart() {
		return ZERO;
	}
}
