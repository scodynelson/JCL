package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;

public class VariableStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private TYPE variableValue;

	protected VariableStructImpl(final String name) {
		super(name);
	}

	public static <T extends LispStruct> VariableStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		final VariableStructImpl<T> struct = new VariableStructImpl<>(name);
		symbolPackage.importSymbol(struct);
		struct.setSymbolPackage(symbolPackage);
		return struct;
	}

	public TYPE getVariableValue() {
		return variableValue;
	}

	@Override
	@SuppressWarnings("unchecked")
	public LispStruct setfSymbolValue(final LispStruct newValue) {
		// TODO: This isn't really safe...
		variableValue = (TYPE) newValue;
		return super.setfSymbolValue(newValue);
	}
}
