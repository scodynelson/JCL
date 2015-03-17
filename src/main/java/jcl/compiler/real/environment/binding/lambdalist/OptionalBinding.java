/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class OptionalBinding extends ParameterBinding {

	private static final long serialVersionUID = 3357381481589151323L;

	private final SuppliedPBinding suppliedPBinding;

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm,
	                       final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocation, T.INSTANCE, initForm);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
