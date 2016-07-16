/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import jcl.LispType;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class Binding {

	private final SymbolStruct var;

	private final LispType type;

	public Binding(final SymbolStruct var) {
		this(var, TType.INSTANCE);
	}

	public Binding(final SymbolStruct var, final LispType type) {
		this.var = var;
		this.type = type;
	}

	public SymbolStruct getVar() {
		return var;
	}

	public LispType getType() {
		return type;
	}
}
