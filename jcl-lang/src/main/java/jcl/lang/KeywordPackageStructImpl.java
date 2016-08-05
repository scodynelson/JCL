/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.lang.internal.KeywordStructImpl;

/**
 * The {@link KeywordPackageStructImpl} is the object representation of a Lisp 'package' type specific for 'keyword'
 * symbols.
 */
public final class KeywordPackageStructImpl extends PackageStructImpl {

	/**
	 * Singleton instance of the Keyword package.
	 */
	public static final PackageStructImpl INSTANCE = new KeywordPackageStructImpl();

	/**
	 * Public constructor.
	 */
	private KeywordPackageStructImpl() {
		super("KEYWORD");
	}

	/**
	 * {@inheritDoc}
	 * Locates the keyword matching the provided {@code symbolName} or creates a new internal keyword with it, interns
	 * it into the package, and returns it with it's package location type.
	 *
	 * @param symbolName
	 * 		the name of the symbol to intern
	 *
	 * @return the keyword if found and it's package location type, or a new keyword with internal type if not found
	 */
	@Override
	public PackageSymbolStruct intern(final String symbolName) {
		final PackageSymbolStruct foundPackageSymbol = findSymbol(symbolName);
		if (foundPackageSymbol != null) {
			return foundPackageSymbol;
		}

		final KeywordStruct symbolStruct = KeywordStructImpl.valueOf(symbolName);
		externalSymbols.put(symbolName, symbolStruct);
		symbolStruct.setSymbolPackage(this);
		return new PackageSymbolStruct(symbolStruct, INTERNAL_KEYWORD);
	}
}
