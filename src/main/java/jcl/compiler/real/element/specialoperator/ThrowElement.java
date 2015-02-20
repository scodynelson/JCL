/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
