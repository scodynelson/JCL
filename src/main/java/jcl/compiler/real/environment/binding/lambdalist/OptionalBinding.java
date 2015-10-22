/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class OptionalBinding extends ParameterBinding {

	private static final long serialVersionUID = 3357381481589151323L;

	private final SuppliedPBinding suppliedPBinding;

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm, final SuppliedPBinding suppliedPBinding) {
		this(symbolStruct, initForm, false, suppliedPBinding);
	}

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm, final boolean isSpecial,
	                       final SuppliedPBinding suppliedPBinding) {
		this(symbolStruct, null, initForm, isSpecial, suppliedPBinding);
	}

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                       final LispStruct initForm, final SuppliedPBinding suppliedPBinding) {
		this(symbolStruct, destructuringForm, initForm, false, suppliedPBinding);
	}

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                       final LispStruct initForm, final boolean isSpecial, final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
