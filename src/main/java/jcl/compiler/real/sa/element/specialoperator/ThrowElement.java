/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.Element;

public class ThrowElement implements Element {

	private static final long serialVersionUID = 935019872276115270L;

	private final Element catchTag;
	private final Element resultForm;

	public ThrowElement(final Element catchTag, final Element resultForm) {
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	public Element getCatchTag() {
		return catchTag;
	}

	public Element getResultForm() {
		return resultForm;
	}
}
