package jcl.lang;

import jcl.lang.internal.DoubleFloatStructImpl;

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

	static DoubleFloatStruct toLispFloat(final float value) {
		return toLispFloat((double) value);
	}

	static DoubleFloatStruct toLispFloat(final Float value) {
		return toLispFloat(value.doubleValue());
	}

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

	static DoubleFloatStruct toLispFloat(final Double value) {
		return toLispFloat(value.doubleValue());
	}

	@Override
	default FloatStruct imagPart() {
		return ZERO;
	}
}
