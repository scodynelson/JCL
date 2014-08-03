package jcl.structs.numbers;

import jcl.variables.LispVariable;

/**
 * The {@link ReadBaseVariable} is the object representation of the *read-base* variable.
 */
public class ReadBaseVariable implements LispVariable<Integer> {

	public static final ReadBaseVariable INSTANCE = new ReadBaseVariable(10);

	private int value;

	/**
	 * Private constructor.
	 *
	 * @param value the read base value
	 */
	private ReadBaseVariable(final int value) {
		this.value = value;
	}

	@Override
	public Integer getValue() {
		return value;
	}

	/**
	 * Setter for the read base value.
	 *
	 * @param value the new read base value
	 */
	public void setValue(final int value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return "ReadBaseVariable{"
				+ "value=" + value
				+ '}';
	}
}
