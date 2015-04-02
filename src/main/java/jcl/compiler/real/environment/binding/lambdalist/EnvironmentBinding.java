/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.List;

public class EnvironmentBinding extends ParameterBinding {

	private static final long serialVersionUID = 7762075133466719180L;

	public EnvironmentBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation) {
		super(symbolStruct, allocation, List.INSTANCE, null);
	}
}
