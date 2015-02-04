/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.Element;

import java.util.List;

public class MultipleValueCallElement implements Element {

	private static final long serialVersionUID = 2789725049143539321L;

	// TODO: do we do evaluation in the SA to product an actual 'FunctionStruct' object here?
	private final Element functionForm;
	private final List<Element> forms;

	public MultipleValueCallElement(final Element functionForm, final List<Element> forms) {
		this.functionForm = functionForm;
		this.forms = forms;
	}

	public Element getFunctionForm() {
		return functionForm;
	}

	public List<Element> getForms() {
		return forms;
	}
}
