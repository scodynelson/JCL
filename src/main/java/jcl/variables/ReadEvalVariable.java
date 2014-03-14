package jcl.variables;

public class ReadEvalVariable implements LispVariable<Boolean> {

	public static final ReadEvalVariable INSTANCE = new ReadEvalVariable(true);

	private boolean value;

	private ReadEvalVariable(final boolean value) {
		this.value = value;
	}

	public Boolean getValue() {
		return value;
	}

	public void setValue(final boolean value) {
		this.value = value;
	}
}
