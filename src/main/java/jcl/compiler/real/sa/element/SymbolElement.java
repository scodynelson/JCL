/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

public class SymbolElement<T extends LispStruct> implements Element {

	private static final long serialVersionUID = 6848146265489259786L;

	private final SymbolStruct<T> symbolStruct;

	public SymbolElement(final SymbolStruct<T> symbolStruct) {
		this.symbolStruct = symbolStruct;
	}

	public SymbolStruct<T> getSymbolStruct() {
		return symbolStruct;
	}
}
