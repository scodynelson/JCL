/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class CatchStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -1022768814372160089L;

	private final LispStruct catchTag;

	private final PrognStruct forms;

	public CatchStruct(final LispStruct catchTag, final PrognStruct forms) {
		this.catchTag = catchTag;
		this.forms = forms;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public PrognStruct getForms() {
		return forms;
	}
}
