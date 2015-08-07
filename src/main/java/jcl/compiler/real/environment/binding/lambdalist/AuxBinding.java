/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;

public class AuxBinding extends ParameterBinding {

	private static final long serialVersionUID = 6424631350711831345L;

	public AuxBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm) {
		this(symbolStruct, initForm, false);
	}

	public AuxBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm, final boolean isSpecial) {
		this(symbolStruct, null, initForm, isSpecial);
	}

	public AuxBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                  final LispStruct initForm) {
		this(symbolStruct, destructuringForm, initForm, false);
	}

	public AuxBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                  final LispStruct initForm, final boolean isSpecial) {
		super(symbolStruct, destructuringForm, TType.INSTANCE, initForm, isSpecial);
	}
}
