package jcl.lang.internal;

import jcl.lang.PackageStruct;

public final class SpecialOperatorStructImpl extends SymbolStructImpl {

	private SpecialOperatorStructImpl(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}

	public static SpecialOperatorStructImpl valueOf(final String name, final PackageStruct symbolPackage) {
		return new SpecialOperatorStructImpl(name, symbolPackage);
	}
}
