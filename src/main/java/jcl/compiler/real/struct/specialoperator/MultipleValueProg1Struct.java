/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class MultipleValueProg1Struct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -1036080843176598388L;

	private final LispStruct firstForm;

	private final PrognStruct forms;

	public MultipleValueProg1Struct(final LispStruct firstForm, final PrognStruct forms) {
		this.firstForm = firstForm;
		this.forms = forms;
	}

	public LispStruct getFirstForm() {
		return firstForm;
	}

	public PrognStruct getForms() {
		return forms;
	}
}
