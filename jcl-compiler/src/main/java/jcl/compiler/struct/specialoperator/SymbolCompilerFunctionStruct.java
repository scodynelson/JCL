/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.lang.SymbolStruct;

public class SymbolCompilerFunctionStruct implements CompilerFunctionStruct {

	private final SymbolStruct functionSymbol;

	public SymbolCompilerFunctionStruct(final SymbolStruct functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolStruct getFunctionSymbol() {
		return functionSymbol;
	}
}
