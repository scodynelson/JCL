/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LocallyEnvironment extends LexicalEnvironment {

	private static final long serialVersionUID = 8519831785716648716L;

	public LocallyEnvironment(final LexicalEnvironment parent, final int closureDepth) {
		super(parent, Marker.LOCALLY, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
