/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class BodyBinding extends ParameterBinding {

	private static final long serialVersionUID = 491035484834969841L;

	public BodyBinding(final SymbolStruct<?> var) {
		this(var, false);
	}

	public BodyBinding(final SymbolStruct<?> var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public BodyBinding(final SymbolStruct<?> var, final DestructuringLambdaListBindings destructuringForm) {
		this(var, destructuringForm, false);
	}

	public BodyBinding(final SymbolStruct<?> var, final DestructuringLambdaListBindings destructuringForm,
	                   final boolean isSpecial) {
		super(var, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}
}
