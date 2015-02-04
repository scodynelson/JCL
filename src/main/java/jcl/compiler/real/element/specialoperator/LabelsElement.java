/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;

import java.util.List;

public class LabelsElement extends InnerFunctionElement {

	private static final long serialVersionUID = -2347494500321073144L;

	public LabelsElement(final List<LabelsVar> vars, final List<Element> forms, final LexicalEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class LabelsVar extends InnerFunctionVar {

		private static final long serialVersionUID = 2989214415282349607L;

		public LabelsVar(final SymbolElement<?> var, final Element initForm) {
			super(var, initForm);
		}
	}
}
