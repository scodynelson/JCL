/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.types.typespecifiers.TypeSpecifier;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class TheStruct implements LispStruct {

	private static final long serialVersionUID = -8054543185157500625L;

	private final TypeSpecifier valueType;

	private final LispStruct form;

	public TheStruct(final TypeSpecifier valueType, final LispStruct form) {
		this.valueType = valueType;
		this.form = form;
	}

	public TypeSpecifier getValueType() {
		return valueType;
	}

	public LispStruct getForm() {
		return form;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
