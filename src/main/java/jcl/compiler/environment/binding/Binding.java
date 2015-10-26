/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import java.io.Serializable;

import jcl.LispType;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class Binding implements Serializable {

	private static final long serialVersionUID = 5776546981120800982L;

	private final SymbolStruct<?> var;

	private final LispType type;

	public Binding(final SymbolStruct<?> var) {
		this(var, TType.INSTANCE);
	}

	public Binding(final SymbolStruct<?> var, final LispType type) {
		this.var = var;
		this.type = type;
	}

	public SymbolStruct<?> getVar() {
		return var;
	}

	public LispType getType() {
		return type;
	}
}
