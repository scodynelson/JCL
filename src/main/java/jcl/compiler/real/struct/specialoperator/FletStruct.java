/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.FletEnvironment;
import jcl.compiler.real.struct.specialoperator.FletStruct.FletVar;
import jcl.symbols.SymbolStruct;

public class FletStruct extends InnerFunctionStruct<FletEnvironment, FletVar> {

	private static final long serialVersionUID = 3770382068803341963L;

	public FletStruct(final List<FletVar> vars, final List<LispStruct> forms, final FletEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class FletVar extends InnerFunctionStruct.InnerFunctionVar {

		private static final long serialVersionUID = -794246121764492302L;

		public FletVar(final SymbolStruct<?> var, final CompilerFunctionStruct initForm) {
			super(var, initForm);
		}
	}
}
