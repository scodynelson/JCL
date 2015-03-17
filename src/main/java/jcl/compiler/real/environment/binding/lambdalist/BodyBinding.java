/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.List;

public class BodyBinding extends ParameterBinding {

	private static final long serialVersionUID = 491035484834969841L;

	public BodyBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm) {
		super(symbolStruct, allocation, List.INSTANCE, initForm);
	}
}
