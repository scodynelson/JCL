/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LetEnvironment extends BindingEnvironment {

	private static final long serialVersionUID = -6810260653724112416L;

	public LetEnvironment(final Environment parent, final int closureDepth) {
		super(parent, Marker.LET, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
