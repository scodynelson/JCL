/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.SymbolStruct;

public class EnvironmentParameter extends Parameter {

	public EnvironmentParameter(final SymbolStruct var) {
		super(var, null, null, true);
	}
}
