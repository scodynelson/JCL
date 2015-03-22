package jcl.symbols;

import jcl.LispStruct;
import jcl.packages.PackageStruct;

public class Variable<T extends LispStruct> extends SymbolStruct<T> {

	private static final long serialVersionUID = -7762556245735490337L;

	public Variable(final String name, final PackageStruct symbolPackage, final T value) {
		super(name, symbolPackage, value);
	}
}
