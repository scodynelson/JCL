/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.FletElement.FletVar;
import jcl.compiler.real.environment.FletEnvironment;

import java.util.List;

public class FletElement extends InnerFunctionElement<FletEnvironment, FletVar> {

	private static final long serialVersionUID = 3770382068803341963L;

	public FletElement(final List<FletVar> vars, final List<Element> forms, final FletEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class FletVar extends InnerFunctionElement.InnerFunctionVar {

		private static final long serialVersionUID = -794246121764492302L;

		public FletVar(final SymbolElement<?> var, final Element initForm) {
			super(var, initForm);
		}
	}
}