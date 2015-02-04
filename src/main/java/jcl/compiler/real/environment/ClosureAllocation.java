/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ClosureAllocation implements Allocation {

	private static final long serialVersionUID = 8918948643826465949L;

	private final Closure closure;

	public ClosureAllocation(final Closure closure) {
		this.closure = closure;
	}

	public Closure getClosure() {
		return closure;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
