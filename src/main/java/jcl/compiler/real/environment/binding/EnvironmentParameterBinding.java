/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.symbols.SymbolStruct;

public class EnvironmentParameterBinding extends EnvironmentBinding<ParameterAllocation> {

	private static final long serialVersionUID = -4911545990664188285L;

	private final LispStruct initForm;

	public EnvironmentParameterBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispType type,
	                                   final LispStruct initForm) {
		super(symbolStruct, allocation, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}
}
