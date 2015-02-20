/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class GoElement<T extends Element> implements Element {

	private static final long serialVersionUID = -4331758400526441262L;

	private final Element tag;

	public GoElement(final T tag) {
		this.tag = tag;
	}

	public Element getTag() {
		return tag;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
