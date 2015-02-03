/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.symbols.SymbolStruct;

public class SymbolFunctionElement implements FunctionElement {

	private static final long serialVersionUID = 1450935885516226944L;

	private final SymbolStruct<?> functionSymbol;

	public SymbolFunctionElement(final SymbolStruct<?> functionSymbol) {
		this.functionSymbol = functionSymbol;
	}
}
