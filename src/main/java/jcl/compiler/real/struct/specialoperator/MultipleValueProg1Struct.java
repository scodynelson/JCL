/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;

public class MultipleValueProg1Struct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -1036080843176598388L;

	private final LispStruct firstForm;

	private final List<LispStruct> forms;

	public MultipleValueProg1Struct(final LispStruct firstForm, final List<LispStruct> forms) {
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
