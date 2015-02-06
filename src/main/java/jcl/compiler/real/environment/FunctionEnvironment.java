/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class FunctionEnvironment extends LexicalEnvironment {

	private static final long serialVersionUID = -5882720704455871085L;

	protected FunctionEnvironment(final Environment parent, final Marker marker, final int closureDepth) {
		super(parent, marker, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
