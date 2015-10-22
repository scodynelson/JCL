/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class WholeBinding extends ParameterBinding {

	private static final long serialVersionUID = 1375721535668777719L;

	public WholeBinding(final SymbolStruct<?> var) {
		this(var, false);
	}

	public WholeBinding(final SymbolStruct<?> var, final boolean isSpecial) {
		super(var, null, ListType.INSTANCE, null, isSpecial);
	}
}
