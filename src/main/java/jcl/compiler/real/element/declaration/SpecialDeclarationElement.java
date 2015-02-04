/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.declaration;

import jcl.compiler.real.element.SymbolElement;

public class SpecialDeclarationElement implements DeclarationElement {

	private static final long serialVersionUID = 7353657327204677544L;

	private final SymbolElement<?> var;

	public SpecialDeclarationElement(final SymbolElement<?> var) {
		this.var = var;
	}

	public SymbolElement<?> getVar() {
		return var;
	}
}
