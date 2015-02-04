/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.SymbolElement;

public class GoSymbolElement implements GoElement {

	private static final long serialVersionUID = -6696260185148126193L;

	private final SymbolElement<?> tag;

	public GoSymbolElement(final SymbolElement<?> tag) {
		this.tag = tag;
	}

	public SymbolElement<?> getTag() {
		return tag;
	}
}
