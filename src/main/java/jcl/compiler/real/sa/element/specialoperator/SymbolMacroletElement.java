/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.SymbolElement;

import java.io.Serializable;
import java.util.List;

public class SymbolMacroletElement implements Element {

	private static final long serialVersionUID = 2362123144166759037L;

	private final List<SymbolMacroletElementVar> vars;
	private final List<Element> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public SymbolMacroletElement(final List<SymbolMacroletElementVar> vars, final List<Element> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<SymbolMacroletElementVar> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public static class SymbolMacroletElementVar implements Serializable {

		private static final long serialVersionUID = -601687250765470819L;

		private final SymbolElement<?> var;
		private final Element expansion;

		public SymbolMacroletElementVar(final SymbolElement<?> var, final Element expansion) {
			this.var = var;
			this.expansion = expansion;
		}

		public SymbolElement<?> getVar() {
			return var;
		}

		public Element getExpansion() {
			return expansion;
		}
	}
}
