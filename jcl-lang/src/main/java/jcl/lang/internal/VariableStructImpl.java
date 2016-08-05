package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;

public class VariableStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private TYPE variableValue;

	protected VariableStructImpl(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		variableValue = value;
	}

	public static <T extends LispStruct> VariableStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage, final T value) {
		return new VariableStructImpl<>(name, symbolPackage, value);
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
