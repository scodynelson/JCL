package jcl.symbols;

import jcl.LispStruct;
import jcl.packages.PackageStruct;

public class VariableStruct<TYPE extends LispStruct> extends SymbolStruct {

	private TYPE variableValue;

	public VariableStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		variableValue = value;
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
