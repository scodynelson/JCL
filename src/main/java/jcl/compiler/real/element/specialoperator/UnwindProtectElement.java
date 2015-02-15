/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;

import java.util.List;

public class UnwindProtectElement implements Element {

	private static final long serialVersionUID = 2849602976511423223L;

	private final Element protectedForm;
	private final List<Element> cleanupForms;

	public UnwindProtectElement(final Element protectedForm, final List<Element> cleanupForms) {
		this.protectedForm = protectedForm;
		this.cleanupForms = cleanupForms;
	}

	public Element getProtectedForm() {
		return protectedForm;
	}

	public List<Element> getCleanupForms() {
		return cleanupForms;
	}
}