/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class CatchStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct catchTag;

	private final PrognStruct forms;

	public CatchStruct(final LispStruct catchTag, final List<LispStruct> forms) {
		this.catchTag = catchTag;
		this.forms = new PrognStruct(forms);
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public PrognStruct getForms() {
		return forms;
	}
}
