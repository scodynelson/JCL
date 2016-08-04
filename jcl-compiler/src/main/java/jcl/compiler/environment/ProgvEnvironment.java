/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment;

import jcl.lang.SymbolStructImpl;

public class ProgvEnvironment extends Environment {

	public ProgvEnvironment(final Environment parent) {
		super(parent);
	}

	@Override
	public boolean hasLexicalBinding(final SymbolStructImpl var) {
		return getParent().hasLexicalBinding(var);
	}
}
