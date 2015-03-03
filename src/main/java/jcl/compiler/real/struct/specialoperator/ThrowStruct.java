/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ThrowStruct implements LispStruct {

	private static final long serialVersionUID = 935019872276115270L;

	private final LispStruct catchTag;

	private final LispStruct resultForm;

	public ThrowStruct(final LispStruct catchTag, final LispStruct resultForm) {
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public LispStruct getResultForm() {
		return resultForm;
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
