/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.MacroletElement.MacroletVar;
import jcl.compiler.real.environment.MacroletEnvironment;

import java.util.List;

public class MacroletElement extends InnerFunctionElement<MacroletEnvironment, MacroletVar> {

	private static final long serialVersionUID = -6865772116422991356L;

	public MacroletElement(final List<MacroletVar> vars, final List<Element> forms, final MacroletEnvironment lexicalEnvironment) {
		super(vars, forms, lexicalEnvironment);
	}

	public static class MacroletVar extends InnerFunctionElement.InnerFunctionVar {

		private static final long serialVersionUID = -169311089356148669L;

		public MacroletVar(final SymbolElement<?> var, final Element initForm) {
			super(var, initForm);
		}
	}
}
