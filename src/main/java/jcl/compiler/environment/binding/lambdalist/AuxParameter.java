/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class AuxParameter extends Parameter {

	public AuxParameter(final SymbolStruct var, final LispStruct initForm) {
		this(var, initForm, false);
	}

	public AuxParameter(final SymbolStruct var, final LispStruct initForm, final boolean isSpecial) {
		this(var, null, initForm, isSpecial);
	}

	public AuxParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm) {
		this(var, destructuringForm, initForm, false);
	}

	public AuxParameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final boolean isSpecial) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
	}
}
