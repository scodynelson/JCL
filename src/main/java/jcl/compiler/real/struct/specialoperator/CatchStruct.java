/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class CatchStruct implements LispStruct {

	private static final long serialVersionUID = -1022768814372160089L;

	private final LispStruct catchTag;

	private final List<LispStruct> forms;

	public CatchStruct(final LispStruct catchTag, final List<LispStruct> forms) {
		this.catchTag = catchTag;
		this.forms = forms;
	}

	public LispStruct getCatchTag() {
		return catchTag;
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
