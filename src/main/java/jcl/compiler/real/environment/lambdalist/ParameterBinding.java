/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;

public class ParameterBinding extends Binding {

	private static final long serialVersionUID = 4128878993186537174L;

	private final LispStruct initForm;

	public ParameterBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final Scope scope,
	                        final LispType type, final LispStruct initForm) {
		super(symbolStruct, allocation, scope, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}
}
