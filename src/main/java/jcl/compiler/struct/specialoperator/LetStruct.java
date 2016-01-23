/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.symbols.SymbolStruct;

public class LetStruct extends ClosureCreationStruct<LetStruct.LetVar> {

	public LetStruct(final List<LetVar> vars, final PrognStruct forms, final Environment letEnvironment) {
		super(vars, forms, letEnvironment);
	}

	public static class LetVar {

		private final SymbolStruct var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetVar(final SymbolStruct var, final LispStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public LispStruct getInitForm() {
			return initForm;
		}

		public boolean isSpecial() {
			return isSpecial;
		}
	}
}
