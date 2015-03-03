/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.lists.NullStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class IfStruct implements LispStruct {

	private static final long serialVersionUID = 9172097134073138710L;

	private final LispStruct testForm;

	private final LispStruct thenForm;

	private final LispStruct elseForm;

	public IfStruct(final LispStruct testForm, final LispStruct thenForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		elseForm = NullStruct.INSTANCE;
	}

	public IfStruct(final LispStruct testForm, final LispStruct thenForm, final LispStruct elseForm) {
		this.testForm = testForm;
		this.thenForm = thenForm;
		this.elseForm = elseForm;
	}

	public LispStruct getTestForm() {
		return testForm;
	}

	public LispStruct getThenForm() {
		return thenForm;
	}

	public LispStruct getElseForm() {
		return elseForm;
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
