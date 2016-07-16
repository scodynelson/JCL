/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class MultipleValueCallStruct extends CompilerSpecialOperatorStruct {

	private final CompilerFunctionStruct functionForm;

	private final List<LispStruct> forms;

	public MultipleValueCallStruct(final CompilerFunctionStruct functionForm, final List<LispStruct> forms) {
		this.functionForm = functionForm;
		this.forms = forms;
	}

	public CompilerFunctionStruct getFunctionForm() {
		return functionForm;
	}

	public List<LispStruct> getForms() {
		return forms;
	}
}
