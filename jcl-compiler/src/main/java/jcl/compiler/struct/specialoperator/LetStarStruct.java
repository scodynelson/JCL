/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

public class LetStarStruct extends ClosureCreationStruct<LetStarStruct.LetStarVar> {

	public LetStarStruct(final List<LetStarVar> vars, final PrognStruct forms, final Environment letStarEnvironment) {
		super(vars, forms, letStarEnvironment);
	}

	public static class LetStarVar {

		private final SymbolStruct var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetStarVar(final SymbolStruct var, final LispStruct initForm, final boolean isSpecial) {
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