/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class RequiredParameter extends Parameter {

	private static final long serialVersionUID = 2544143727820268303L;

	public RequiredParameter(final SymbolStruct<?> var) {
		this(var, false);
	}

	public RequiredParameter(final SymbolStruct<?> var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public RequiredParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm) {
		this(var, destructuringForm, false);
	}

	public RequiredParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                         final boolean isSpecial) {
		super(var, destructuringForm, TType.INSTANCE, null, isSpecial);
	}
}
