/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.element.Element;
import jcl.symbols.NILStruct;

public class IfElement implements Element {

	private static final long serialVersionUID = 9172097134073138710L;

	private final LispStruct testForm;
	private final LispStruct thenForm;
	private final LispStruct elseForm;

	public IfElement(final LispStruct testForm, final LispStruct thenForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		elseForm = NILStruct.INSTANCE;
	}

	public IfElement(final LispStruct testForm, final LispStruct thenForm, final LispStruct elseForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		this.elseForm = elseForm;
	}

	public LispStruct getTestForm() {
		return testForm;
	}

	public LispStruct getThenForm() {
		return thenForm;
	}

	public LispStruct getElseForm() {
		return elseForm;
	}
}
