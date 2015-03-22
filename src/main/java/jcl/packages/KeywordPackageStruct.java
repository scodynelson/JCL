/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.packages;

import jcl.symbols.KeywordStruct;
import jcl.system.CommonLispSymbols;

/**
 * The {@link KeywordPackageStruct} is the object representation of a Lisp 'package' type specific for 'keyword'
 * symbols.
 */
final class KeywordPackageStruct extends PackageStruct {

	/**
	 * Singleton instance of the Keyword package.
	 */
	public static final PackageStruct INSTANCE = new KeywordPackageStruct();

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 5527737254864167846L;

	/**
	 * Public constructor.
	 */
	private KeywordPackageStruct() {
		super("KEYWORD");
	}

	@Override
	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final KeywordStruct symbolStruct = new KeywordStruct(symbolName);
		externalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, CommonLispSymbols.INTERNAL);
	}
}
