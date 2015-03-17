/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.T;

public class KeyBinding extends ParameterBinding {

	private static final long serialVersionUID = -8247621420473541525L;

	private final KeywordSymbolStruct keyName;

	private final SuppliedPBinding suppliedPBinding;

	public KeyBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm,
	                  final KeywordSymbolStruct keyName, final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, allocation, T.INSTANCE, initForm);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public KeywordSymbolStruct getKeyName() {
		return keyName;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
