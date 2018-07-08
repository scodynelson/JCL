/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.type.TType;

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

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append(getVar());

		final LispStruct initForm = getInitForm();
		if (!initForm.eq(NILStruct.INSTANCE)) {
			builder.append(initForm);
		}

		return builder.toString();
	}
}
