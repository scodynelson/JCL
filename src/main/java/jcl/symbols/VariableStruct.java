package jcl.symbols;

import jcl.LispStruct;
import jcl.packages.PackageStruct;

public class VariableStruct<TYPE extends LispStruct> extends SymbolStruct<TYPE> {

	private static final long serialVersionUID = -7762556245735490337L;

	public VariableStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
	}
}
