/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function.expanders;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.expander.SymbolMacroExpanderInter;

public abstract class SymbolMacroExpander extends MacroExpander<LispStruct, SymbolStruct> implements SymbolMacroExpanderInter {

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

//	@Override
//	public LispStruct apply(final LispStruct... lispStructs) {
//		final SymbolStruct symbolStruct = (SymbolStruct) lispStructs[0];
//		final Environment environment = (Environment) lispStructs[1];
//		return expand(symbolStruct, environment);
//	}
}
