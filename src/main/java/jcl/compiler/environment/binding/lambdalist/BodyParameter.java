/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.symbols.SymbolStruct;
import jcl.types.ListType;

public class BodyParameter extends Parameter {

	private static final long serialVersionUID = 491035484834969841L;

	public BodyParameter(final SymbolStruct<?> var) {
		this(var, false);
	}

	public BodyParameter(final SymbolStruct<?> var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public BodyParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm) {
		this(var, destructuringForm, false);
	}

	public BodyParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                     final boolean isSpecial) {
		super(var, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}
}
