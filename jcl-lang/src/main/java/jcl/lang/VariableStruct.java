package jcl.lang;

public class VariableStruct<TYPE extends LispStruct> extends SymbolStruct {

	private TYPE variableValue;

	protected VariableStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		variableValue = value;
	}

	public static <T extends LispStruct> VariableStruct<T> valueOf(final String name, final PackageStruct symbolPackage, final T value) {
		return new VariableStruct<>(name, symbolPackage, value);
	}

	public TYPE getVariableValue() {
		return variableValue;
	}

	@Override
	@SuppressWarnings("unchecked")
	public void setValue(final LispStruct value) {
		super.setValue(value);
		// TODO: This isn't really safe...
		variableValue = (TYPE) value;
	}
}
