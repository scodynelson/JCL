/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class FunctionCallElement implements Element {

	private static final long serialVersionUID = 2676444242188589421L;

	private final boolean isRecursiveCall;

	private final SymbolStruct<?> functionSymbol;
	private final List<LispStruct> arguments;

	public FunctionCallElement(final boolean isRecursiveCall, final SymbolStruct<?> functionSymbol, final List<LispStruct> arguments) {
		this.isRecursiveCall = isRecursiveCall;
		this.functionSymbol = functionSymbol;
		this.arguments = arguments;
	}
}
