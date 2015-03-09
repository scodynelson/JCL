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

public class MultipleValueCallStruct implements LispStruct {

	private static final long serialVersionUID = 2789725049143539321L;

	// TODO: do we do evaluation in the SA to product an actual 'FunctionStruct' object here?
	private final LispStruct functionForm;

	private final List<LispStruct> forms;

	public MultipleValueCallStruct(final LispStruct functionForm, final List<LispStruct> forms) {
		this.functionForm = functionForm;
		this.forms = forms;
	}

	public LispStruct getFunctionForm() {
		return functionForm;
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
