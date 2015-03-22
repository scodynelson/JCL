/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.allocation;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class PositionAllocation implements Allocation {

	private static final long serialVersionUID = -1200453833243116544L;

	private final int position;

	protected PositionAllocation(final int position) {
		this.position = position;
	}

	public int getPosition() {
		return position;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(position)
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
		final PositionAllocation rhs = (PositionAllocation) obj;
		return new EqualsBuilder().append(position, rhs.position)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(position)
		                                                                .toString();
	}
}
