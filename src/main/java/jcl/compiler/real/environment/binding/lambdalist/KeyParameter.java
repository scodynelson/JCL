/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class KeyParameter extends Parameter {

	private static final long serialVersionUID = -8247621420473541525L;

	private final SymbolStruct<?> keyName;

	private final SuppliedPParameter suppliedPBinding;

	public KeyParameter(final SymbolStruct<?> var, final LispStruct initForm, final SymbolStruct<?> keyName,
	                    final SuppliedPParameter suppliedPBinding) {
		this(var, initForm, false, keyName, suppliedPBinding);
	}

	public KeyParameter(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial,
	                    final SymbolStruct<?> keyName, final SuppliedPParameter suppliedPBinding) {
		this(var, null, initForm, isSpecial, keyName, suppliedPBinding);
	}

	public KeyParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final SymbolStruct<?> keyName, final SuppliedPParameter suppliedPBinding) {
		this(var, destructuringForm, initForm, false, keyName, suppliedPBinding);
	}

	public KeyParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final boolean isSpecial, final SymbolStruct<?> keyName,
	                    final SuppliedPParameter suppliedPBinding) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public SymbolStruct<?> getKeyName() {
		return keyName;
	}

	public SuppliedPParameter getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
