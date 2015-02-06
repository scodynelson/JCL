/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.SymbolMacroletEnvironment;

import java.io.Serializable;
import java.util.List;

public class SymbolMacroletElement implements Element {

	private static final long serialVersionUID = 2362123144166759037L;

	private final List<SymbolMacroletElementVar> vars;

	private final List<Element> forms;

	private final SymbolMacroletEnvironment symbolMacroletEnvironment;

	public SymbolMacroletElement(final List<SymbolMacroletElementVar> vars, final List<Element> forms, final SymbolMacroletEnvironment symbolMacroletEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.symbolMacroletEnvironment = symbolMacroletEnvironment;
	}

	public List<SymbolMacroletElementVar> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public SymbolMacroletEnvironment getSymbolMacroletEnvironment() {
		return symbolMacroletEnvironment;
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
