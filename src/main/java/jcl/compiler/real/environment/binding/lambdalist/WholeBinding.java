/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class WholeBinding extends ParameterBinding {

	private static final long serialVersionUID = 1375721535668777719L;

	public WholeBinding(final SymbolStruct<?> symbolStruct) {
		this(symbolStruct, false);
	}

	public WholeBinding(final SymbolStruct<?> symbolStruct, final boolean isSpecial) {
		super(symbolStruct, null, ListType.INSTANCE, null, isSpecial);
	}
}
