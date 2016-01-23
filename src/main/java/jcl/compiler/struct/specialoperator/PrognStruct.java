/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class PrognStruct extends CompilerSpecialOperatorStruct {

	private final List<LispStruct> forms;

	public PrognStruct(final List<LispStruct> forms) {
		this.forms = forms;
	}

	public List<LispStruct> getForms() {
		return forms;
	}
}
