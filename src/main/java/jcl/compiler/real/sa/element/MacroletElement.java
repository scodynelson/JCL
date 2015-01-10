/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class MacroletElement extends InnerFunctionElement {

	public MacroletElement(final List<MacroletVar> vars, final List<LispStruct> forms, final Environment environment) {
		super(vars, forms, environment);
	}

	public static class MacroletVar extends InnerFunctionVar {

		public MacroletVar(final SymbolStruct<?> var, final LispStruct initForm) {
			super(var, initForm);
		}
	}
}
