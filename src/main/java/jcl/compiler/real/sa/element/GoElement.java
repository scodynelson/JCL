/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.symbols.SymbolStruct;

public class GoElement implements Element {

	private final SymbolStruct<?> tag;

	public GoElement(final SymbolStruct<?> tag) {
		this.tag = tag;
	}
}
