/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander.real;

import jcl.compiler.real.element.Element;

public class NewMacroExpandReturn {

	private final Element expandedForm;

	private final boolean wasExpanded;

	public NewMacroExpandReturn(final Element expandedForm, final boolean wasExpanded) {
		this.expandedForm = expandedForm;
		this.wasExpanded = wasExpanded;
	}

	public Element getExpandedForm() {
		return expandedForm;
	}

	public boolean wasExpanded() {
		return wasExpanded;
	}
}
