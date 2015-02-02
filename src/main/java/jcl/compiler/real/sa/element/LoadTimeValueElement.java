/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

public class LoadTimeValueElement implements Element {

	private static final long serialVersionUID = 1106008809581974795L;

	private final String name;

	private final LispStruct value;

	public LoadTimeValueElement(final String name, final LispStruct value) {
		this.name = name;
		this.value = value;
	}
}
