/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.SymbolElement;

public class SymbolFunctionElement implements FunctionElement {

	private static final long serialVersionUID = 1450935885516226944L;

	private final SymbolElement<?> functionSymbol;

	public SymbolFunctionElement(final SymbolElement<?> functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolElement<?> getFunctionSymbol() {
		return functionSymbol;
	}
}
