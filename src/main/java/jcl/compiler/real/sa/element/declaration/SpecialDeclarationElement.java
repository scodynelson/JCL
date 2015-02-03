/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.declaration;

import jcl.symbols.SymbolStruct;

public class SpecialDeclarationElement implements DeclarationElement {

	private static final long serialVersionUID = 7353657327204677544L;

	private final SymbolStruct<?> var;

	public SpecialDeclarationElement(final SymbolStruct<?> var) {
		this.var = var;
	}

	public SymbolStruct<?> getVar() {
		return var;
	}
}
