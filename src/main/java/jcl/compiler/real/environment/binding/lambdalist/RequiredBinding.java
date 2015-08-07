/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class RequiredBinding extends ParameterBinding {

	private static final long serialVersionUID = 2544143727820268303L;

	public RequiredBinding(final SymbolStruct<?> symbolStruct) {
		this(symbolStruct, false);
	}

	public RequiredBinding(final SymbolStruct<?> symbolStruct, final boolean isSpecial) {
		this(symbolStruct, null, isSpecial);
	}

	public RequiredBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm) {
		this(symbolStruct, destructuringForm, false);
	}

	public RequiredBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                       final boolean isSpecial) {
		super(symbolStruct, destructuringForm, TType.INSTANCE, null, isSpecial);
	}
}
