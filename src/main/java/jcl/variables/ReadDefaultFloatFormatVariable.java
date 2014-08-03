package jcl.variables;

import jcl.types.Float;
import jcl.types.SingleFloat;

/**
 * The {@link ReadDefaultFloatFormatVariable} is the object representation of the *read-default-float-format* variable.
 */
public class ReadDefaultFloatFormatVariable implements LispVariable<Float> {

	public static final ReadDefaultFloatFormatVariable INSTANCE = new ReadDefaultFloatFormatVariable(SingleFloat.INSTANCE);

	private Float value;

	/**
	 * Private constructor.
	 *
	 * @param value the float format value
	 */
	private ReadDefaultFloatFormatVariable(final Float value) {
		this.value = value;
	}

	@Override
	public Float getValue() {
		return value;
	}

	/**
	 * Setter for the float format value.
	 *
	 * @param value the new float format value
	 */
	public void setValue(final Float value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return "ReadDefaultFloatFormatVariable{"
				+ "value=" + value
				+ '}';
	}
}
