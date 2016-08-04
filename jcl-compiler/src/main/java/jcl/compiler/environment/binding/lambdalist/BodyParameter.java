/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.SymbolStructImpl;
import jcl.type.ListType;

public class BodyParameter extends Parameter {

	public BodyParameter(final SymbolStructImpl var) {
		this(var, false);
	}

	public BodyParameter(final SymbolStructImpl var, final boolean isSpecial) {
		this(var, null, isSpecial);
	}

	public BodyParameter(final SymbolStructImpl var, final DestructuringLambdaList destructuringForm) {
		this(var, destructuringForm, false);
	}

	public BodyParameter(final SymbolStructImpl var, final DestructuringLambdaList destructuringForm,
	                     final boolean isSpecial) {
		super(var, destructuringForm, ListType.INSTANCE, null, isSpecial);
	}
}
