/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class RestParameter extends Parameter {

	private static final long serialVersionUID = 5070599837585531277L;

	public RestParameter(final SymbolStruct<?> var) {
		this(var, false);
	}

	public RestParameter(final SymbolStruct<?> var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public RestParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm) {
		this(var, destructuringForm, false);
	}

	public RestParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                     final boolean isSpecial) {
		super(var, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}
}
