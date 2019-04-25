/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class WholeParameter extends Parameter {

	public WholeParameter(final SymbolStruct var) {
		this(var, false);
	}

	public WholeParameter(final SymbolStruct var, final boolean isSpecial) {
		super(var, null, CommonLispSymbols.T, null, isSpecial);
	}
}
