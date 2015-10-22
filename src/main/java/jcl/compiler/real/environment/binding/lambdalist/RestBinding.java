/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class RestBinding extends ParameterBinding {

	private static final long serialVersionUID = 5070599837585531277L;

	public RestBinding(final SymbolStruct<?> var) {
		this(var, false);
	}

	public RestBinding(final SymbolStruct<?> var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public RestBinding(final SymbolStruct<?> var, final DestructuringLambdaListBindings destructuringForm) {
		this(var, destructuringForm, false);
	}

	public RestBinding(final SymbolStruct<?> var, final DestructuringLambdaListBindings destructuringForm,
	                   final boolean isSpecial) {
		super(var, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}
}
