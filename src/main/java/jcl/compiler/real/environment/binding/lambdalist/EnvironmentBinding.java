/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class EnvironmentBinding extends ParameterBinding {

	private static final long serialVersionUID = 7762075133466719180L;

	public EnvironmentBinding(final SymbolStruct<?> var) {
		super(var, null, ListType.INSTANCE, null, true);
	}
}
