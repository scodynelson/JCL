/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> {

	private static final long serialVersionUID = -4041262906159677088L;

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct<?> functionSymbol = getFunctionSymbol();
		functionSymbol.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(listStruct, environment);
	}
}
