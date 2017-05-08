package jcl.lang;

import jcl.lang.internal.DoubleFloatStructImpl;

public interface DoubleFloatStruct extends FloatStruct {

	static DoubleFloatStruct toLispFloat(final Double value) {
		return new DoubleFloatStructImpl(value);
	}
}
