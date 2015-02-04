/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;

import java.util.List;

public class CatchElement implements Element {

	private static final long serialVersionUID = -1022768814372160089L;

	private final Element catchTag;
	private final List<Element> forms;

	public CatchElement(final Element catchTag, final List<Element> forms) {
		this.catchTag = catchTag;
		this.forms = forms;
	}

	public Element getCatchTag() {
		return catchTag;
	}

	public List<Element> getForms() {
		return forms;
	}
}
