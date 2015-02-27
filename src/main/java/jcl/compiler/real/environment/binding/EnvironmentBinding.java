/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.allocation.Allocation;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class EnvironmentBinding<A extends Allocation> extends Binding<A> {

	private static final long serialVersionUID = 2910922877559341453L;

	protected EnvironmentBinding(final SymbolElement symbolStruct, final A allocation, final LispType type) {
		super(symbolStruct, allocation, type);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
