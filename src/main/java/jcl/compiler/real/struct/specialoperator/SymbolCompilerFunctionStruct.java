/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.symbols.SymbolStruct;

public class SymbolCompilerFunctionStruct implements CompilerFunctionStruct {

	private static final long serialVersionUID = 1450935885516226944L;

	private final SymbolStruct<?> functionSymbol;

	public SymbolCompilerFunctionStruct(final SymbolStruct<?> functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolStruct<?> getFunctionSymbol() {
		return functionSymbol;
	}
}
