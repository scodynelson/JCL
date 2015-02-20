/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class MultipleValueProg1Element implements Element {

	private static final long serialVersionUID = -1036080843176598388L;

	private final Element firstForm;
	private final List<Element> forms;

	public MultipleValueProg1Element(final Element firstForm, final List<Element> forms) {
		this.firstForm = firstForm;
		this.forms = forms;
	}

	public Element getFirstForm() {
		return firstForm;
	}

	public List<Element> getForms() {
		return forms;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
