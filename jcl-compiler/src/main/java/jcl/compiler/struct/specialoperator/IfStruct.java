/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class IfStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct testForm;

	private final LispStruct thenForm;

	private final LispStruct elseForm;

	public IfStruct(final LispStruct testForm, final LispStruct thenForm, final LispStruct elseForm) {
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

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("(IF ");

		final String testFormPrinted = testForm.toString();
		builder.append(testFormPrinted);
		builder.append(' ');
		final String thenFormPrinted = thenForm.toString();
		builder.append(thenFormPrinted);
		builder.append(' ');
		final String elseFormPrinted = elseForm.toString();
		builder.append(elseFormPrinted);
		builder.append(')');

		return builder.toString();
	}
}
