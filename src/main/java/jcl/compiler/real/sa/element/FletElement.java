/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class FletElement extends InnerFunctionElement {

	private static final long serialVersionUID = 3770382068803341963L;

	public FletElement(final List<FletVar> vars, final List<LispStruct> forms, final Environment environment) {
		super(vars, forms, environment);
	}

	public static class FletVar extends InnerFunctionVar {

		public FletVar(final SymbolStruct<?> var, final LispStruct initForm) {
			super(var, initForm);
		}
	}
}
