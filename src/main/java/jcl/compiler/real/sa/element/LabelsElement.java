/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class LabelsElement extends InnerFunctionElement {

	private static final long serialVersionUID = -2347494500321073144L;

	public LabelsElement(final List<LabelsVar> vars, final List<LispStruct> forms, final Environment environment) {
		super(vars, forms, environment);
	}

	public static class LabelsVar extends InnerFunctionVar {

		public LabelsVar(final SymbolStruct<?> var, final LispStruct initForm) {
			super(var, initForm);
		}
	}
}
