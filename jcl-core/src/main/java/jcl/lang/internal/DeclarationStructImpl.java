package jcl.lang.internal;

import jcl.lang.PackageStruct;

public final class DeclarationStructImpl extends SymbolStructImpl {

	private DeclarationStructImpl(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
		init();
	}

	/**
	 * Post construction method.
	 */
	private void init() {
		value = this;
	}

	public static DeclarationStructImpl valueOf(final String name, final PackageStruct symbolPackage) {
		return new DeclarationStructImpl(name, symbolPackage);
	}
}
