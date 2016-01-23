/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment;

import jcl.symbols.SymbolStruct;

public class ProgvEnvironment extends Environment {

	public ProgvEnvironment(final Environment parent) {
		super(parent);
	}

	@Override
	public boolean hasLexicalBinding(final SymbolStruct var) {
		return getParent().hasLexicalBinding(var);
	}
}
