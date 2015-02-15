/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.InnerFunctionElement.InnerFunctionVar;
import jcl.compiler.real.environment.Environment;

import java.io.Serializable;
import java.util.List;

public abstract class InnerFunctionElement<E extends Environment, V extends InnerFunctionVar> implements Element {

	private static final long serialVersionUID = -535794959428263863L;

	private final List<V> vars;

	private final List<Element> forms;

	private final E lexicalEnvironment;

	InnerFunctionElement(final List<V> vars, final List<Element> forms, final E lexicalEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.lexicalEnvironment = lexicalEnvironment;
	}

	public List<V> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public E getLexicalEnvironment() {
		return lexicalEnvironment;
	}

	public static class InnerFunctionVar implements Serializable {

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
