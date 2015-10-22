/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.binding.Binding;
import jcl.symbols.SymbolStruct;

public class ParameterBinding extends Binding {

	private static final long serialVersionUID = 4128878993186537174L;

	private final LispStruct initForm;

	private final DestructuringLambdaListBindings destructuringForm;

	private final boolean isSpecial;

	protected ParameterBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                           final LispType type, final LispStruct initForm, final boolean isSpecial) {
		super(symbolStruct, type);
		this.destructuringForm = destructuringForm;
		this.initForm = initForm;
		this.isSpecial = isSpecial;
	}

	public DestructuringLambdaListBindings getDestructuringForm() {
		return destructuringForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	public boolean isSpecial() {
		return isSpecial;
	}
}
