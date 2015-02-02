/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class DynamicEnvironment extends Environment<DynamicEnvironment> {

	public static final DynamicEnvironment FREE = new DynamicEnvironment(null);

	private static final long serialVersionUID = 4931675023340550160L;

	// TODO: load-time-value ???
	public DynamicEnvironment(final DynamicEnvironment parent) {
		super(parent);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
