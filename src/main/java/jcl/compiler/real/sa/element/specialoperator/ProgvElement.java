/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.environment.DynamicEnvironment;
import jcl.compiler.real.sa.element.Element;
import jcl.compiler.real.sa.element.SymbolElement;

import java.io.Serializable;
import java.util.List;

public class ProgvElement implements Element {

	private static final long serialVersionUID = 6286708668973616872L;

	private final List<ProgvVar> vars;
	private final List<Element> forms;

	private final DynamicEnvironment dynamicEnvironment;

	public ProgvElement(final List<ProgvVar> vars, final List<Element> forms, final DynamicEnvironment dynamicEnvironment) {
		this.vars = vars;
		this.forms = forms;
		this.dynamicEnvironment = dynamicEnvironment;
	}

	public List<ProgvVar> getVars() {
		return vars;
	}

	public List<Element> getForms() {
		return forms;
	}

	public DynamicEnvironment getDynamicEnvironment() {
		return dynamicEnvironment;
	}

	public static class ProgvVar implements Serializable {

		private static final long serialVersionUID = -5131005121770228469L;

		private final SymbolElement<?> var;
		private final Element val;

		public ProgvVar(final SymbolElement<?> var, final Element val) {
			this.var = var;
			this.val = val;
		}

		public SymbolElement<?> getVar() {
			return var;
		}

		public Element getVal() {
			return val;
		}
	}
}
