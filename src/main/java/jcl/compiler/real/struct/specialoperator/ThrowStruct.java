/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class ThrowStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 935019872276115270L;

	private final LispStruct catchTag;

	private final LispStruct resultForm;

	public ThrowStruct(final LispStruct catchTag, final LispStruct resultForm) {
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public LispStruct getResultForm() {
		return resultForm;
	}
}
