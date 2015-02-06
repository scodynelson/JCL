/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LabelsEnvironment extends InnerFunctionEnvironment {

	private static final long serialVersionUID = 8774938340814484890L;

	public LabelsEnvironment(final Environment parent, final int closureDepth) {
		super(parent, Marker.LABELS, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
