/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class FletEnvironment extends InnerFunctionEnvironment {

	private static final long serialVersionUID = -5894291977824149893L;

	public FletEnvironment(final Environment parent, final int closureDepth) {
		super(parent, Marker.FLET, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
