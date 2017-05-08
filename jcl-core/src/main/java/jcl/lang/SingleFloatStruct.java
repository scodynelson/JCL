package jcl.lang;

import jcl.lang.internal.SingleFloatStructImpl;

public interface SingleFloatStruct extends FloatStruct {

	static SingleFloatStruct toLispFloat(final Float value) {
		return new SingleFloatStructImpl(value);
	}
}
