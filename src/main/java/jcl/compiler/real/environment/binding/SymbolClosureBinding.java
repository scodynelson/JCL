/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

public class SymbolClosureBinding extends SymbolBinding {

	private static final long serialVersionUID = -5306882046173256938L;

	public SymbolClosureBinding(final SymbolStruct<?> symbolStruct, final LispType type,
	                            final Environment binding) {
		super(symbolStruct, type, binding);
	}
}
