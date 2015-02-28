/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.types.typespecifiers.TypeSpecifier;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class TheElement implements Element {

	private static final long serialVersionUID = -8054543185157500625L;

	private final TypeSpecifier valueType;

	private final Element form;

	public TheElement(final TypeSpecifier valueType, final Element form) {
		this.valueType = valueType;
		this.form = form;
	}

	public TypeSpecifier getValueType() {
		return valueType;
	}

	public Element getForm() {
		return form;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
