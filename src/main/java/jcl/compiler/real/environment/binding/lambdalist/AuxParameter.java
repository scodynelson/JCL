/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class AuxParameter extends Parameter {

	private static final long serialVersionUID = 6424631350711831345L;

	public AuxParameter(final SymbolStruct<?> var, final LispStruct initForm) {
		this(var, initForm, false);
	}

	public AuxParameter(final SymbolStruct<?> var, final LispStruct initForm, final boolean isSpecial) {
		this(var, null, initForm, isSpecial);
	}

	public AuxParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm) {
		this(var, destructuringForm, initForm, false);
	}

	public AuxParameter(final SymbolStruct<?> var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final boolean isSpecial) {
		super(var, destructuringForm, TType.INSTANCE, initForm, isSpecial);
	}
}
