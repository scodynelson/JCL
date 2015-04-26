/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class RestBinding extends ParameterBinding {

	private static final long serialVersionUID = 5070599837585531277L;

	public RestBinding(final SymbolStruct<?> symbolStruct) {
		this(symbolStruct, false);
	}

	public RestBinding(final SymbolStruct<?> symbolStruct, final boolean isSpecial) {
		super(symbolStruct, ListType.INSTANCE, null, isSpecial);
	}
}
