/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * Internal class for returning a {@link SymbolStruct} and it's current package symbol type as a {@link KeywordStructImpl}.
 */
public class PackageSymbolStruct {

	/**
	 * The {@link SymbolStruct} symbol.
	 */
	private final SymbolStruct symbol;

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
	PackageSymbolStruct(final SymbolStruct symbol, final KeywordStructImpl packageSymbolType) {
		this.symbol = symbol;
		this.packageSymbolType = packageSymbolType;
	}

	/**
	 * Getter for package-symbol {@link #symbol} property.
	 *
	 * @return package-symbol {@link #symbol} property
	 */
	public SymbolStruct getSymbol() {
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
