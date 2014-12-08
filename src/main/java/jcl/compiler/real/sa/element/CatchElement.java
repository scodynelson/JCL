/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;

public class CatchElement implements Element {

	private final LispStruct catchTag;
	private final List<LispStruct> forms;

	public CatchElement(final LispStruct catchTag, final List<LispStruct> forms) {
		this.catchTag = catchTag;
		this.forms = forms;
	}
}
