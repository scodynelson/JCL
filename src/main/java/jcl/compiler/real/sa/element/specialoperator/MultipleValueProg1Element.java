/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.Element;

import java.util.List;

public class MultipleValueProg1Element implements Element {

	private static final long serialVersionUID = -1036080843176598388L;

	private final Element firstForm;
	private final List<Element> forms;

	public MultipleValueProg1Element(final Element firstForm, final List<Element> forms) {
		this.firstForm = firstForm;
		this.forms = forms;
	}

	public Element getFirstForm() {
		return firstForm;
	}

	public List<Element> getForms() {
		return forms;
	}
}
