/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * Internal class for returning a {@link SymbolStructImpl} and it's current package symbol type as a {@link KeywordStructImpl}.
 */
public class PackageSymbolStruct {

	/**
	 * The {@link SymbolStructImpl} symbol.
	 */
	private final SymbolStructImpl symbol;

	/**
	 * The {@link #symbol}s package location type.
	 */
	private final KeywordStructImpl packageSymbolType;

	/**
	 * Package protected constructor.
	 *
	 * @param symbol
	 * 		the symbol result
	 * @param packageSymbolType
	 * 		the symbol package location
	 */
	PackageSymbolStruct(final SymbolStructImpl symbol, final KeywordStructImpl packageSymbolType) {
		this.symbol = symbol;
		this.packageSymbolType = packageSymbolType;
	}

	/**
	 * Getter for package-symbol {@link #symbol} property.
	 *
	 * @return package-symbol {@link #symbol} property
	 */
	public SymbolStructImpl getSymbol() {
		return symbol;
	}

	/**
	 * Getter for package-symbol {@link #packageSymbolType} property.
	 *
	 * @return package-symbol {@link #packageSymbolType} property
	 */
	public KeywordStructImpl getPackageSymbolType() {
		return packageSymbolType;
	}
}
