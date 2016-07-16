/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.symbols.SymbolStruct;

public abstract class SymbolMacroExpander extends MacroExpander<LispStruct, SymbolStruct> {

	protected SymbolMacroExpander(final String documentation) {
		super(documentation);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return null;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
	}


	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final SymbolStruct symbolStruct = (SymbolStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(symbolStruct, environment);
	}
}
