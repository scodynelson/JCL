/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.real.environment.Environment;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;

public class TestGroundMacroFunctionExpanderGenerator extends MacroFunctionExpander<LispStruct> {

	private static final long serialVersionUID = -1939696402314939143L;

	@Override
	public LispStruct expand(final ListStruct form, final Environment environment) {
		return new TestGroundMacroFunctionExpanderInnerFunction().apply(form, environment);
	}

	private class TestGroundMacroFunctionExpanderInnerFunction extends FunctionStruct {

		private static final long serialVersionUID = -5371992564511489062L;

		@Override
		public LispStruct apply(final LispStruct... lispStructs) {
			return new CharacterStruct(97);
		}
	}
}
