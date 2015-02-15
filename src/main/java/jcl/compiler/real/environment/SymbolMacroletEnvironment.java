/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolMacroletEnvironment extends Environment {

	private static final long serialVersionUID = 1037128089833502867L;

	public SymbolMacroletEnvironment(final Environment parent, final int closureDepth) {
		super(parent, Marker.SYMBOL_MACROLET, closureDepth);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
