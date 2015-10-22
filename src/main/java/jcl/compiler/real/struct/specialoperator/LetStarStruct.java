/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

public class LetStarStruct extends ClosureCreationStruct<LetStarStruct.LetStarVar> {

	private static final long serialVersionUID = -3186671381163635893L;

	public LetStarStruct(final List<LetStarVar> vars, final PrognStruct forms, final Environment letStarEnvironment) {
		super(vars, forms, letStarEnvironment);
	}

	public static class LetStarVar implements Serializable {

		private static final long serialVersionUID = 3246152127057600416L;

		private final SymbolStruct<?> var;

		private final LispStruct initForm;

		private final boolean isSpecial;

		public LetStarVar(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial) {
			this.var = var;
			this.initForm = initForm;
			this.isSpecial = isSpecial;
		}

		public SymbolStruct<?> getVar() {
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
