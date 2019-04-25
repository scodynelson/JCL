/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class EnvironmentParameter extends Parameter {

	public EnvironmentParameter(final SymbolStruct var) {
		super(var, null, CommonLispSymbols.T, null, true);
	}
}
