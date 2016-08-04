/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.lang.SymbolStructImpl;

public class SymbolCompilerFunctionStruct implements CompilerFunctionStruct {

	private final SymbolStructImpl functionSymbol;

	public SymbolCompilerFunctionStruct(final SymbolStructImpl functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolStructImpl getFunctionSymbol() {
		return functionSymbol;
	}
}
