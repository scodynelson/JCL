package jcl.variables;

public class ReadSuppressVariable implements LispVariable<Boolean> {

	public static final ReadSuppressVariable INSTANCE = new ReadSuppressVariable(false);

	private boolean value;

	private ReadSuppressVariable(final boolean value) {
		this.value = value;
	}

	public Boolean getValue() {
		return value;
	}

	public void setValue(final boolean value) {
		this.value = value;
	}
}
