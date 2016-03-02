/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.Environment;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.symbols.SymbolStruct;

@SuppressWarnings({"all", "rawtypes"})
public class TestGroundSymbolMacroExpander extends SymbolMacroExpander {

	@Override
	public LispStruct expand(final SymbolStruct form, final Environment environment) {
		return CharacterStruct.valueOf(123456789);
	}
}
