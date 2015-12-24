/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.io.Serializable;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class SymbolMacroletStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 2362123144166759037L;

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

	public static class SymbolMacroletVar implements Serializable {

		private static final long serialVersionUID = -601687250765470819L;

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
