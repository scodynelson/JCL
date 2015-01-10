/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.declaration;

import jcl.symbols.SymbolStruct;

public class SpecialDeclarationElement extends DeclarationElement {

	private final SymbolStruct<?> var;

	public SpecialDeclarationElement(final SymbolStruct<?> var) {
		this.var = var;
	}

	public SymbolStruct<?> getVar() {
		return var;
	}
}
