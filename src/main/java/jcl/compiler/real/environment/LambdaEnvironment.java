/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -1182568685360839544L;

	private int parameterNumber = -1;

	public LambdaEnvironment(final Environment parent, final int closureDepth) {
		super(parent, closureDepth);
	}

	public int getNextParameterNumber() {
		return parameterNumber++;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
