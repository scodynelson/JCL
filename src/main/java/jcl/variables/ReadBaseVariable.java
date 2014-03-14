package jcl.variables;

public class ReadBaseVariable implements LispVariable<Integer> {

	public static final ReadBaseVariable INSTANCE = new ReadBaseVariable(10);

	private int value;

	private ReadBaseVariable(final int value) {
		this.value = value;
	}

	public Integer getValue() {
		return value;
	}

	public void setValue(final int value) {
		this.value = value;
	}
}
