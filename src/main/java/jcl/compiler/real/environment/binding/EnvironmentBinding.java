/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.allocation.Allocation;
import jcl.symbols.SymbolStruct;

public abstract class EnvironmentBinding<A extends Allocation> extends Binding<A> {

	private static final long serialVersionUID = 2910922877559341453L;

	protected EnvironmentBinding(final SymbolStruct<?> symbolStruct, final A allocation, final LispType type) {
		super(symbolStruct, allocation, type);
	}
}
