/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;

public class QuoteElement implements Element {

	private static final long serialVersionUID = 2854755653951600124L;

	private final Element object;

	public QuoteElement(final Element object) {
		this.object = object;
	}

	public Element getObject() {
		return object;
	}
}