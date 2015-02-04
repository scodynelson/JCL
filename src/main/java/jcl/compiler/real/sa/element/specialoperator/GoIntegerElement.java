/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.IntegerElement;

public class GoIntegerElement implements GoElement {

	private static final long serialVersionUID = 6515586661046207604L;

	private final IntegerElement tag;

	public GoIntegerElement(final IntegerElement tag) {
		this.tag = tag;
	}

	public IntegerElement getTag() {
		return tag;
	}
}
