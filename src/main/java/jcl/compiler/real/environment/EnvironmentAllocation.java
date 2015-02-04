/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentAllocation implements Allocation {

	private static final long serialVersionUID = 1294755562180249532L;

	private final LexicalEnvironment environment;

	public EnvironmentAllocation(final LexicalEnvironment environment) {
		this.environment = environment;
	}

	public LexicalEnvironment getEnvironment() {
		return environment;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
