/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.SymbolStructImpl;
import jcl.type.ListType;

public class EnvironmentParameter extends Parameter {

	public EnvironmentParameter(final SymbolStructImpl var) {
		super(var, null, ListType.INSTANCE, null, true);
	}
}
