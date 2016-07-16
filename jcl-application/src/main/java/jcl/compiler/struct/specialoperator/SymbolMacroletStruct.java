/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

public class SymbolMacroletStruct extends CompilerSpecialOperatorStruct {

	private final List<SymbolMacroletVar> vars;

	private final PrognStruct forms;

	private final Environment symbolMacroletEnvironment;

	public SymbolMacroletStruct(final List<SymbolMacroletVar> vars, final PrognStruct forms, final Environment symbolMacroletEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.symbolMacroletEnvironment = symbolMacroletEnvironment;
	}

	public List<SymbolMacroletVar> getVars() {
		return vars;
	}

	public PrognStruct getForms() {
		return forms;
	}

	public Environment getSymbolMacroletEnvironment() {
		return symbolMacroletEnvironment;
	}

	public static class SymbolMacroletVar {

		private final SymbolStruct var;

		private final LispStruct expansion;

		public SymbolMacroletVar(final SymbolStruct var, final LispStruct expansion) {
			this.var = var;
			this.expansion = expansion;
		}

		public SymbolStruct getVar() {
			return var;
		}

		public LispStruct getExpansion() {
			return expansion;
		}
	}
}
