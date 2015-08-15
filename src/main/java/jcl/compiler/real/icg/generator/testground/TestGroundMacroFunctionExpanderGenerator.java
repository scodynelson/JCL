/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.Environment;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;

public class TestGroundMacroFunctionExpanderGenerator extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = -1939696402314939143L;

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		return new CharacterStruct(97);
	}
}
