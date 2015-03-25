/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.io.Serializable;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LoadTimeValue implements Serializable {

	private static final long serialVersionUID = -3900591459449629077L;

	private final String uniqueLTVId;

	private final LispStruct value;

	public LoadTimeValue(final String uniqueLTVId, final LispStruct value) {
		this.uniqueLTVId = uniqueLTVId;
		this.value = value;
	}

	public String getUniqueLTVId() {
		return uniqueLTVId;
	}

	public LispStruct getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(uniqueLTVId)
		                            .append(value)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final LoadTimeValue rhs = (LoadTimeValue) obj;
		return new EqualsBuilder().append(uniqueLTVId, rhs.uniqueLTVId)
		                          .append(value, rhs.value)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(uniqueLTVId)
		                                                                .append(value)
		                                                                .toString();
	}
}
