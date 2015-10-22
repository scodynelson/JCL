/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class KeyBinding extends ParameterBinding {

	private static final long serialVersionUID = -8247621420473541525L;

	private final SymbolStruct<?> keyName;

	private final SuppliedPBinding suppliedPBinding;

	public KeyBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm,
	                  final SymbolStruct<?> keyName, final SuppliedPBinding suppliedPBinding) {
		this(symbolStruct, initForm, false, keyName, suppliedPBinding);
	}

	public KeyBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm, final boolean isSpecial,
	                  final SymbolStruct<?> keyName, final SuppliedPBinding suppliedPBinding) {
		this(symbolStruct, null, initForm, isSpecial, keyName, suppliedPBinding);
	}

	public KeyBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                  final LispStruct initForm, final SymbolStruct<?> keyName, final SuppliedPBinding suppliedPBinding) {
		this(symbolStruct, destructuringForm, initForm, false, keyName, suppliedPBinding);
	}

	public KeyBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                  final LispStruct initForm, final boolean isSpecial, final SymbolStruct<?> keyName,
	                  final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, destructuringForm, TType.INSTANCE, initForm, isSpecial);
		this.keyName = keyName;
		this.suppliedPBinding = suppliedPBinding;
	}

	public SymbolStruct<?> getKeyName() {
		return keyName;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}
}
