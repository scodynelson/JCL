package jcl.variables;

import jcl.types.Float;
import jcl.types.SingleFloat;

public class ReadDefaultFloatFormatVariable implements LispVariable<Float> {

	public static final ReadDefaultFloatFormatVariable INSTANCE = new ReadDefaultFloatFormatVariable(SingleFloat.INSTANCE);

	private Float value;

	private ReadDefaultFloatFormatVariable(final Float value) {
		this.value = value;
	}

	public Float getValue() {
		return value;
	}

	public void setValue(final Float value) {
		this.value = value;
	}
}
