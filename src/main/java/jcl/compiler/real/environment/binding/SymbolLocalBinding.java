/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.allocation.LocalAllocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolLocalBinding extends SymbolBinding<LocalAllocation> {

	private static final long serialVersionUID = 2199826721253003696L;

	public SymbolLocalBinding(final SymbolStruct<?> symbolStruct, final LocalAllocation allocation, final Scope scope,
	                          final LispType type, final Environment<?> binding) {
		super(symbolStruct, allocation, scope, type, binding);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
