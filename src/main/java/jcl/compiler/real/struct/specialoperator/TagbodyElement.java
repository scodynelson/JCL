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
import java.util.Map;

public class TagbodyElement implements LispStruct {

	private static final long serialVersionUID = -2970777170741142162L;

	private final Map<LispStruct, List<LispStruct>> tagbodyForms;

	public TagbodyElement(final Map<LispStruct, List<LispStruct>> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<LispStruct, List<LispStruct>> getTagbodyForms() {
		return tagbodyForms;
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
