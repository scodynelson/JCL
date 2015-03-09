/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.io.Serializable;
import java.util.UUID;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LoadTimeValue implements Serializable {

	private static final long serialVersionUID = -3900591459449629077L;

	private final UUID uniqueLTVId;

	private final LispStruct value;

	public LoadTimeValue(final UUID uniqueLTVId, final LispStruct value) {
		this.uniqueLTVId = uniqueLTVId;
		this.value = value;
	}

	public UUID getUniqueLTVId() {
		return uniqueLTVId;
	}

	public LispStruct getValue() {
		return value;
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
