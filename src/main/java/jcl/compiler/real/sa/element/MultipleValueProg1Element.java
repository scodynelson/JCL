/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;

public class MultipleValueProg1Element implements Element {

	private static final long serialVersionUID = -1036080843176598388L;

	private final LispStruct firstForm;
	private final List<LispStruct> forms;

	public MultipleValueProg1Element(final LispStruct firstForm, final List<LispStruct> forms) {
		this.firstForm = firstForm;
		this.forms = forms;
	}

	public LispStruct getFirstForm() {
		return firstForm;
	}

	public List<LispStruct> getForms() {
		return forms;
	}
}
