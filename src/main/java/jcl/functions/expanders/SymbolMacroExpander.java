/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

public abstract class SymbolMacroExpander<O extends LispStruct> extends MacroExpander<O, SymbolStruct<?>> {

	private static final long serialVersionUID = -4579665130389126919L;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final SymbolStruct<?> symbolStruct = (SymbolStruct<?>) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(symbolStruct, environment);
	}
}
