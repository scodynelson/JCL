/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class MacroletElement extends InnerFunctionElement {

	private static final long serialVersionUID = -6865772116422991356L;

	public MacroletElement(final List<MacroletVar> vars, final List<LispStruct> forms, final LexicalEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class MacroletVar extends InnerFunctionVar {

		public MacroletVar(final SymbolStruct<?> var, final LispStruct initForm) {
			super(var, initForm);
		}
	}
}
