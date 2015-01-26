package jcl.symbols;

import jcl.LispStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.PackageStruct;

public final class Constant<T extends LispStruct> extends SymbolStruct<T> {

	private static final long serialVersionUID = -2257699556001691329L;

	public Constant(final String name, final PackageStruct symbolPackage, final T value) {
		super(name, symbolPackage, value);
	}

	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		throw new RuntimeException("Can't set package for constant " + name + '.');
	}

	@Override
	public void setValue(final T value) {
		throw new RuntimeException("Can't set value for constant " + name + '.');
	}

	@Override
	public void setFunction(final FunctionStruct function) {
		throw new RuntimeException("Can't set function for constant " + name + '.');
	}

	@Override
	public void setProperty(final LispStruct key, final LispStruct value) {
		throw new RuntimeException("Can't set properties for constant " + name + '.');
	}
}
