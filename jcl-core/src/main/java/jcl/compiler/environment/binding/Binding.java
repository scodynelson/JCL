/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class Binding {

	private final SymbolStruct var;

	private final LispStruct type;

	public Binding(final SymbolStruct var) {
		this(var, CommonLispSymbols.T);
	}

	public Binding(final SymbolStruct var, final LispStruct type) {
		this.var = var;
		this.type = type;
	}

	public SymbolStruct getVar() {
		return var;
	}

	public LispStruct getType() {
		return type;
	}

	@Override
	public String toString() {
		return var.toString();
	}
}
