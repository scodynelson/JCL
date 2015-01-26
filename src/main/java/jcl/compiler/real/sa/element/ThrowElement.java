/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

public class ThrowElement implements Element {

	private static final long serialVersionUID = 935019872276115270L;

	private final LispStruct catchTag;
	private final LispStruct resultForm;

	public ThrowElement(final LispStruct catchTag, final LispStruct resultForm) {
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}
}
