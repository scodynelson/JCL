/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.SymbolMacroExpander;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.factory.LispStructFactory;

@SuppressWarnings({"all", "rawtypes"})
public class TestGroundSymbolMacroExpander extends SymbolMacroExpander {

	protected TestGroundSymbolMacroExpander() {
		// TODO: Documentation???
		super("");
	}

	@Override
	public LispStruct expand(final SymbolStructImpl form, final Environment environment) {
		return LispStructFactory.toCharacter(123456789);
	}

	// TODO: COMMENTED OUT?!?!?!?
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}
