/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironment extends FunctionEnvironment {

	private static final long serialVersionUID = -1182568685360839544L;

	public LambdaEnvironment(final LexicalEnvironment parent, final int closureDepth) {
		super(parent, Marker.LAMBDA, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
