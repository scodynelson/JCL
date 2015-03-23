/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.compiler.real.environment.MacroletEnvironment;
import jcl.compiler.real.struct.specialoperator.MacroletStruct.MacroletVar;
import jcl.symbols.SymbolStruct;

public class MacroletStruct extends InnerFunctionStruct<MacroletEnvironment, MacroletVar> {

	private static final long serialVersionUID = -6865772116422991356L;

	public MacroletStruct(final List<MacroletVar> vars, final PrognStruct forms, final MacroletEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class MacroletVar extends InnerFunctionStruct.InnerFunctionVar {

		private static final long serialVersionUID = -169311089356148669L;

		public MacroletVar(final SymbolStruct<?> var, final CompilerFunctionStruct initForm, final boolean isSpecial) {
			super(var, initForm, isSpecial);
		}
	}
}
