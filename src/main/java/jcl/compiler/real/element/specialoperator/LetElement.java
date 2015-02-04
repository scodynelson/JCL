/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;

import java.io.Serializable;
import java.util.List;

public class LetElement implements Element {

	private static final long serialVersionUID = -3186671381163635893L;

	private final List<LetVar> vars;
	private final List<Element> forms;

	private final LexicalEnvironment lexicalEnvironment;

	public LetElement(final List<LetVar> vars, final List<Element> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<LetVar> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public static class LetVar implements Serializable {

		private static final long serialVersionUID = 3246152127057600416L;

		private final SymbolElement<?> var;
		private final Element initForm;

		public LetVar(final SymbolElement<?> var, final Element initForm) {
			this.var = var;
			this.initForm = initForm;
		}

		public SymbolElement<?> getVar() {
			return var;
		}

		public Element getInitForm() {
			return initForm;
		}
	}
}
