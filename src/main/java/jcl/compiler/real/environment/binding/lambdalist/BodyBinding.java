/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class BodyBinding extends ParameterBinding {

	private static final long serialVersionUID = 491035484834969841L;

	public BodyBinding(final SymbolStruct<?> symbolStruct) {
		this(symbolStruct, false);
	}

	public BodyBinding(final SymbolStruct<?> symbolStruct, final boolean isSpecial) {
		this(symbolStruct, null, isSpecial);
	}

	public BodyBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm) {
		this(symbolStruct, destructuringForm, false);
	}

	public BodyBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                   final boolean isSpecial) {
		super(symbolStruct, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}
}
