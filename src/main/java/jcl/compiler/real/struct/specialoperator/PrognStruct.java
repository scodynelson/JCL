/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;

public class PrognStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 2712116484918089080L;

	private final List<LispStruct> forms;

	public PrognStruct(final List<LispStruct> forms) {
		this.forms = forms;
	}

	public List<LispStruct> getForms() {
		return forms;
	}
}
