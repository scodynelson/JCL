/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.symbols.SymbolStruct;

public class ParameterBinding extends Binding<ParameterAllocation> {

	private static final long serialVersionUID = 4128878993186537174L;

	private final LispStruct initForm;

	protected ParameterBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispType type,
	                           final LispStruct initForm) {
		super(symbolStruct, allocation, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}
}
