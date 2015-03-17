/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class SuppliedPBinding extends ParameterBinding {

	private static final long serialVersionUID = -8399229506171557644L;

	public SuppliedPBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation) {
		super(symbolStruct, allocation, T.INSTANCE, null);
	}
}
