/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public abstract class EnvironmentBinding extends Binding {

	private static final long serialVersionUID = 2910922877559341453L;

	protected EnvironmentBinding(final SymbolStruct<?> symbolStruct, final LispType type) {
		super(symbolStruct, type);
	}
}
