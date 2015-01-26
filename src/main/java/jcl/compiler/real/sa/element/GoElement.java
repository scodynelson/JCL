/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

public class GoElement implements Element {

	private static final long serialVersionUID = 8271758465125711307L;

	private final LispStruct tag;

	public GoElement(final LispStruct tag) {
		this.tag = tag;
	}
}
