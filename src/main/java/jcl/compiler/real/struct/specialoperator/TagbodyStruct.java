/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class TagbodyStruct implements LispStruct {

	private static final long serialVersionUID = -2970777170741142162L;

	private final Map<GoStruct<?>, List<LispStruct>> tagbodyForms;

	public TagbodyStruct(final Map<GoStruct<?>, List<LispStruct>> tagbodyForms) {
		this.tagbodyForms = tagbodyForms;
	}

	public Map<GoStruct<?>, List<LispStruct>> getTagbodyForms() {
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
