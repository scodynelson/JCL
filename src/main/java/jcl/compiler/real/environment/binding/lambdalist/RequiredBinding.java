/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class RequiredBinding extends ParameterBinding {

	private static final long serialVersionUID = 2544143727820268303L;

	public RequiredBinding(final SymbolStruct<?> var) {
		this(var, false);
	}

	public RequiredBinding(final SymbolStruct<?> var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public RequiredBinding(final SymbolStruct<?> var, final DestructuringLambdaListBindings destructuringForm) {
		this(var, destructuringForm, false);
	}

	public RequiredBinding(final SymbolStruct<?> var, final DestructuringLambdaListBindings destructuringForm,
	                       final boolean isSpecial) {
		super(var, destructuringForm, TType.INSTANCE, null, isSpecial);
	}
}
