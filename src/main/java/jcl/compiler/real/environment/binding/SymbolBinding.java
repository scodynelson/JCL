/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.Allocation;
import jcl.symbols.SymbolStruct;

public class SymbolBinding<A extends Allocation> extends Binding<A> {

	private static final long serialVersionUID = -3462756070576114237L;

	private final Environment binding;

	protected SymbolBinding(final SymbolStruct<?> symbolStruct, final A allocation, final LispType type,
	                        final Environment binding) {
		super(symbolStruct, allocation, type);
		this.binding = binding;
	}

	public Environment getBinding() {
		return binding;
	}
}
