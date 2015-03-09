/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class MultipleValueProg1Struct implements LispStruct {

	private static final long serialVersionUID = -1036080843176598388L;

	private final LispStruct firstForm;

	private final List<LispStruct> forms;

	public MultipleValueProg1Struct(final LispStruct firstForm, final List<LispStruct> forms) {
		this.firstForm = firstForm;
		this.forms = forms;
	}

	public LispStruct getFirstForm() {
		return firstForm;
	}

	public List<LispStruct> getForms() {
		return forms;
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
