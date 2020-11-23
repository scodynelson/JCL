/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * Internal class for returning a {@link SymbolStruct} and it's current package symbol type as a {@link KeywordStruct}.
 */
@Getter
@AllArgsConstructor
public final class PackageSymbolStruct {

	/**
	 * The {@link SymbolStruct} symbol.
	 */
	private final SymbolStruct symbol;

	/**
	 * The {@link #symbol}s package location type.
	 */
	private final KeywordStruct packageSymbolType;
}
