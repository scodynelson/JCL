/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.allocation;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
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
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
