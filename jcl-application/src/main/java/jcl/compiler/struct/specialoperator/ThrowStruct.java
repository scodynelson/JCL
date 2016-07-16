/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class ThrowStruct extends CompilerSpecialOperatorStruct {

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
