/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class MultipleValueCallElement implements Element {

	private static final long serialVersionUID = 2789725049143539321L;

	// TODO: do we do evaluation in the SA to product an actual 'FunctionStruct' object here?
	private final Element functionForm;
	private final List<Element> forms;

	public MultipleValueCallElement(final Element functionForm, final List<Element> forms) {
		this.functionForm = functionForm;
		this.forms = forms;
	}

	public Element getFunctionForm() {
		return functionForm;
	}

	public List<Element> getForms() {
		return forms;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
