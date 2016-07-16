/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class MultipleValueProg1Struct extends CompilerSpecialOperatorStruct {

	private final LispStruct firstForm;

	private final PrognStruct forms;

	public MultipleValueProg1Struct(final LispStruct firstForm, final List<LispStruct> forms) {
		this.firstForm = firstForm;
		this.forms = new PrognStruct(forms);
	}

	public LispStruct getFirstForm() {
		return firstForm;
	}

	public PrognStruct getForms() {
		return forms;
	}
}
