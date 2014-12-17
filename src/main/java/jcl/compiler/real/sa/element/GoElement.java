/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

public class GoElement implements Element {

	private final LispStruct tag;

	public GoElement(final LispStruct tag) {
		this.tag = tag;
	}
}
