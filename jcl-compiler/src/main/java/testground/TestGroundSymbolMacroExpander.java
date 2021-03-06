/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.SymbolMacroExpander;
import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

@SuppressWarnings({"all", "rawtypes"})
public class TestGroundSymbolMacroExpander extends SymbolMacroExpander {

	protected TestGroundSymbolMacroExpander() {
		// TODO: Documentation???
		super("");
	}

	@Override
	public LispStruct expand(final SymbolStruct form, final Environment environment) {
		return CharacterStruct.toLispCharacter(123456789);
	}

	// TODO: COMMENTED OUT?!?!?!?
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}
