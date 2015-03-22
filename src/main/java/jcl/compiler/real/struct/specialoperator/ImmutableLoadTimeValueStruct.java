/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.UUID;

import jcl.compiler.real.struct.SpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ImmutableLoadTimeValueStruct extends SpecialOperatorStruct implements LoadTimeValueStruct {

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
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(uniqueLTVId)
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
		final ImmutableLoadTimeValueStruct rhs = (ImmutableLoadTimeValueStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(uniqueLTVId, rhs.uniqueLTVId)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(uniqueLTVId)
		                                                                .toString();
	}
}
