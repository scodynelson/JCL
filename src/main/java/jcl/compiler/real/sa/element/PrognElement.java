/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

import java.util.List;

public class PrognElement implements Element {

	private static final long serialVersionUID = 2712116484918089080L;

	private final List<LispStruct> forms;

	public PrognElement(final List<LispStruct> forms) {
		this.forms = forms;
	}
}
