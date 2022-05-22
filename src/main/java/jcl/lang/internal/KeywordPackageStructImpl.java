/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import jcl.lang.KeywordStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;

/**
 * The {@link KeywordPackageStructImpl} is the object representation of a Lisp 'package' type specific for 'keyword'
 * symbols.
 */
public final class KeywordPackageStructImpl extends PackageStructImpl {

	/**
	 * Constant name of the keyword package.
	 */
	private static final String PACKAGE_NAME = "KEYWORD";

	/**
	 * Singleton instance of the Keyword package.
	 */
	public static final PackageStruct INSTANCE = new KeywordPackageStructImpl();

	static {
		GlobalPackageStruct.ALL_PACKAGES.put(PACKAGE_NAME, INSTANCE);
	}

	/**
	 * Public constructor.
	 */
	private KeywordPackageStructImpl() {
		super(PACKAGE_NAME);
	}

	@Override
	protected SymbolStruct internNewSymbol(final String symbolName) {
		final SymbolStruct symbol = KeywordStruct.toLispKeyword(symbolName);
		externalSymbols.put(symbolName, symbol);
		return symbol;
	}
}
