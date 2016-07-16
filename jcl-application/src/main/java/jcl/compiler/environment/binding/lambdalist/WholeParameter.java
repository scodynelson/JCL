/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.SymbolStruct;
import jcl.type.ListType;

public class WholeParameter extends Parameter {

	public WholeParameter(final SymbolStruct var) {
		this(var, false);
	}

	public WholeParameter(final SymbolStruct var, final boolean isSpecial) {
		super(var, null, ListType.INSTANCE, null, isSpecial);
	}
}
