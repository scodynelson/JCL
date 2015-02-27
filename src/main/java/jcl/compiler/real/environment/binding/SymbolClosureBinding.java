/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.ClosureAllocation;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolClosureBinding extends SymbolBinding<ClosureAllocation> {

	private static final long serialVersionUID = -5306882046173256938L;

	public SymbolClosureBinding(final SymbolElement symbolStruct, final ClosureAllocation allocation, final LispType type,
	                            final Environment binding) {
		super(symbolStruct, allocation, type, binding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
