/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.symbols.SymbolStruct;

import java.io.Serializable;
import java.util.List;

public class SymbolMacroletElement implements Element {

	private static final long serialVersionUID = 2362123144166759037L;

	private final List<SymbolMacroletElementVar> vars;
	private final List<LispStruct> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public SymbolMacroletElement(final List<SymbolMacroletElementVar> vars, final List<LispStruct> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<SymbolMacroletElementVar> getVars() {
		return vars;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public static class SymbolMacroletElementVar implements Serializable {

		private static final long serialVersionUID = -601687250765470819L;

		private final SymbolStruct<?> var;
		private final LispStruct expansion;

		public SymbolMacroletElementVar(final SymbolStruct<?> var, final LispStruct expansion) {
			this.var = var;
			this.expansion = expansion;
		}

		public SymbolStruct<?> getVar() {
			return var;
		}

		public LispStruct getExpansion() {
			return expansion;
		}
	}
}
