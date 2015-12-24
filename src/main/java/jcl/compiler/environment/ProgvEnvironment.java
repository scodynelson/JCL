/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment;

import jcl.symbols.SymbolStruct;

public class ProgvEnvironment extends Environment {

	private static final long serialVersionUID = 5583104617030812969L;

	public ProgvEnvironment(final Environment parent) {
		super(parent);
	}

	@Override
	public boolean hasLexicalBinding(final SymbolStruct var) {
		return getParent().hasLexicalBinding(var);
	}
}
