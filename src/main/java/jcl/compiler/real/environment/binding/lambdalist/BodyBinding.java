/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class BodyBinding extends ParameterBinding {

	private static final long serialVersionUID = 491035484834969841L;

	public BodyBinding(final SymbolStruct<?> symbolStruct) {
		this(symbolStruct, false);
	}

	public BodyBinding(final SymbolStruct<?> symbolStruct, final boolean isSpecial) {
		super(symbolStruct, ListType.INSTANCE, null, isSpecial);
	}
}
