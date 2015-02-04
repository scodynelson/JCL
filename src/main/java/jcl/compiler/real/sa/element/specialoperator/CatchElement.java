/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.element.Element;

import java.util.List;

public class CatchElement implements Element {

	private static final long serialVersionUID = -1022768814372160089L;

	private final LispStruct catchTag;
	private final List<LispStruct> forms;

	public CatchElement(final LispStruct catchTag, final List<LispStruct> forms) {
		this.catchTag = catchTag;
		this.forms = forms;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public List<LispStruct> getForms() {
		return forms;
	}
}
