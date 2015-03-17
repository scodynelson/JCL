/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class AuxBinding extends ParameterBinding {

	private static final long serialVersionUID = 6424631350711831345L;

	public AuxBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm) {
		super(symbolStruct, allocation, T.INSTANCE, initForm);
	}
}
