/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.DynamicEnvironment;
import jcl.symbols.SymbolStruct;

import java.io.Serializable;
import java.util.List;

public class ProgvElement implements Element {

	private static final long serialVersionUID = 6286708668973616872L;

	private final List<ProgvVar> vars;
	private final List<LispStruct> forms;

	private final DynamicEnvironment dynamicEnvironment;

	public ProgvElement(final List<ProgvVar> vars, final List<LispStruct> forms, final DynamicEnvironment dynamicEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.dynamicEnvironment = dynamicEnvironment;
	}

	public static class ProgvVar implements Serializable {

		private static final long serialVersionUID = -5131005121770228469L;

		private final SymbolStruct<?> var;
		private final LispStruct val;

		public ProgvVar(final SymbolStruct<?> var, final LispStruct val) {
			this.var = var;
			this.val = val;
		}
	}
}
