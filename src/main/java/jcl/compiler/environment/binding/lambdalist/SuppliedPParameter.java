/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class SuppliedPParameter extends Parameter {

	private static final long serialVersionUID = -8399229506171557644L;

	public SuppliedPParameter(final SymbolStruct<?> var) {
		this(var, false);
	}

	public SuppliedPParameter(final SymbolStruct<?> var, final boolean isSpecial) {
		super(var, null, TType.INSTANCE, null, isSpecial);
	}
}
