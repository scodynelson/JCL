/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.SymbolElement;

import java.util.List;

public class BlockElement implements Element {

	private static final long serialVersionUID = -115779602179582479L;

	private final SymbolElement<?> name;
	private final List<Element> forms;

	public BlockElement(final SymbolElement<?> name, final List<Element> forms) {
		this.name = name;
		this.forms = forms;
	}

	public SymbolElement<?> getName() {
		return name;
	}

	public List<Element> getForms() {
		return forms;
	}
}
