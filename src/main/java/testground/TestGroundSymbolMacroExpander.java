/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.Environment;
import jcl.functions.expanders.SymbolMacroExpander;

@SuppressWarnings({"all", "rawtypes"})
public class TestGroundSymbolMacroExpander extends SymbolMacroExpander {

	private static final long serialVersionUID = -4379776342588838432L;

	@Override
	public LispStruct expand(final LispStruct form, final Environment environment) {
		return new CharacterStruct(123456789);
	}
}
