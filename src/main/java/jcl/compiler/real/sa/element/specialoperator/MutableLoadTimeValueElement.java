/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.LispStruct;

public class MutableLoadTimeValueElement implements LoadTimeValueElement {

	private static final long serialVersionUID = 8088799347738800471L;

	private final LispStruct form;

	public MutableLoadTimeValueElement(final LispStruct form) {
		this.form = form;
	}

	public LispStruct getForm() {
		return form;
	}
}
