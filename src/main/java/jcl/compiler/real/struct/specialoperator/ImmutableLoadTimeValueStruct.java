/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.UUID;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ImmutableLoadTimeValueStruct implements LoadTimeValueStruct {

	private static final long serialVersionUID = 857211495712280441L;

	private final UUID uniqueLTVId;

	public ImmutableLoadTimeValueStruct(final UUID uniqueLTVId) {
		this.uniqueLTVId = uniqueLTVId;
	}

	public UUID getUniqueLTVId() {
		return uniqueLTVId;
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
