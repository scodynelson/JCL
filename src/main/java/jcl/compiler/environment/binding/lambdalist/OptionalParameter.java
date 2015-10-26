/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class OptionalParameter extends Parameter {

	private static final long serialVersionUID = 3357381481589151323L;

	private final SuppliedPParameter suppliedPBinding;

	public OptionalParameter(final SymbolStruct<?> var, final LispStruct initForm, final SuppliedPParameter suppliedPBinding) {
		this(var, initForm, false, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial,
	                         final SuppliedPParameter suppliedPBinding) {
		this(var, null, initForm, isSpecial, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                         final LispStruct initForm, final SuppliedPParameter suppliedPBinding) {
		this(var, destructuringForm, initForm, false, suppliedPBinding);
	}

	public OptionalParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                         final LispStruct initForm, final boolean isSpecial, final SuppliedPParameter suppliedPBinding) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPParameter getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
