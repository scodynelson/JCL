/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class WholeParameter extends Parameter {

	private static final long serialVersionUID = 1375721535668777719L;

	public WholeParameter(final SymbolStruct var) {
		this(var, false);
	}

	public WholeParameter(final SymbolStruct var, final boolean isSpecial) {
		super(var, null, ListType.INSTANCE, null, isSpecial);
	}
}
