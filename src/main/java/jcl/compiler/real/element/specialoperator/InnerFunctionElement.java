/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;

import java.io.Serializable;
import java.util.List;

public abstract class InnerFunctionElement implements Element {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<? extends InnerFunctionVar> vars;
	private final List<Element> forms;

	private final LexicalEnvironment lexicalEnvironment;

	InnerFunctionElement(final List<? extends InnerFunctionVar> vars, final List<Element> forms, final LexicalEnvironment lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<? extends InnerFunctionVar> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public LexicalEnvironment getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public abstract static class InnerFunctionVar implements Serializable {

		private static final long serialVersionUID = 891453745075246590L;

		private final SymbolElement<?> var;
		private final Element initForm;

		InnerFunctionVar(final SymbolElement<?> var, final Element initForm) {
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
