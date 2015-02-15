/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LambdaEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -1182568685360839544L;

	public LambdaEnvironment(final Environment parent, final int closureDepth) {
		super(parent, Marker.LAMBDA, closureDepth);
	}

	// TODO: get rid of this when we can get rid of the 'Marker' enumeration.
	protected LambdaEnvironment(final Environment parent, final Marker marker, final int closureDepth) {
		super(parent, marker, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
