package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;

public class VariableStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private TYPE variableValue;

	protected VariableStructImpl(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		variableValue = value;
	}

	public static <T extends LispStruct> VariableStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		return new VariableStructImpl<>(name, symbolPackage, null);
	}

	public TYPE getVariableValue() {
		return variableValue;
	}

	@Override
	@SuppressWarnings("unchecked")
	public LispStruct setSymbolValue(final LispStruct value) {
		// TODO: This isn't really safe...
		variableValue = (TYPE) value;
		return super.setSymbolValue(value);
	}
}
