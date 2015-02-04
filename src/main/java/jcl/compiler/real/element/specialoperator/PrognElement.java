/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;

import java.util.List;

public class PrognElement implements Element {

	private static final long serialVersionUID = 2712116484918089080L;

	private final List<Element> forms;

	public PrognElement(final List<Element> forms) {
		this.forms = forms;
	}

	public List<Element> getForms() {
		return forms;
	}
}
