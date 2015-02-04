/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;

public class MutableLoadTimeValueElement implements LoadTimeValueElement {

	private static final long serialVersionUID = 8088799347738800471L;

	private final Element form;

	public MutableLoadTimeValueElement(final Element form) {
		this.form = form;
	}

	public Element getForm() {
		return form;
	}
}
