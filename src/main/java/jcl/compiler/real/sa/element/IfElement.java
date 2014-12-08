/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

public class IfElement implements Element {

	private final LispStruct testForm;
	private final LispStruct thenForm;
	private final LispStruct elseForm;
	private final boolean hasElse;

	public IfElement(final LispStruct testForm, final LispStruct thenForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		elseForm = null;
		hasElse = false;
	}

	public IfElement(final LispStruct testForm, final LispStruct thenForm, final LispStruct elseForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		this.elseForm = elseForm;
		hasElse = true;
	}
}
