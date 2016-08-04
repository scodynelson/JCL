/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import jcl.lang.SymbolStructImpl;
import jcl.type.LispType;
import jcl.type.TType;

public class Binding {

	private final SymbolStructImpl var;

	private final LispType type;

	public Binding(final SymbolStructImpl var) {
		this(var, TType.INSTANCE);
	}

	public Binding(final SymbolStructImpl var, final LispType type) {
		this.var = var;
		this.type = type;
	}

	public SymbolStructImpl getVar() {
		return var;
	}

	public LispType getType() {
		return type;
	}
}
