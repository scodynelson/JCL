/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;

public class QuoteElement implements Element {

	private static final long serialVersionUID = 2854755653951600124L;

	private final LispStruct object;

	public QuoteElement(final LispStruct object) {
		this.object = object;
	}

	public LispStruct getObject() {
		return object;
	}
}
