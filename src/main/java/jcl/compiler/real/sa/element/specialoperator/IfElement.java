/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.NullElement;

public class IfElement implements Element {

	private static final long serialVersionUID = 9172097134073138710L;

	private final Element testForm;
	private final Element thenForm;
	private final Element elseForm;

	public IfElement(final Element testForm, final Element thenForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		elseForm = NullElement.INSTANCE;
	}

	public IfElement(final Element testForm, final Element thenForm, final Element elseForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		this.elseForm = elseForm;
	}

	public Element getTestForm() {
		return testForm;
	}

	public Element getThenForm() {
		return thenForm;
	}

	public Element getElseForm() {
		return elseForm;
	}
}
