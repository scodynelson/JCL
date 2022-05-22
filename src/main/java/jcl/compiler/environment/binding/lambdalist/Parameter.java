/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.compiler.environment.binding.Binding;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Parameter extends Binding {

	private LispStruct initForm;
	private Class<? extends LispStruct> initFormClass;
	private final DestructuringLambdaList destructuringForm;
	private final boolean isSpecial;

	protected Parameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                    final LispStruct initForm, final boolean isSpecial) {
		super(var);
		this.destructuringForm = destructuringForm;
		this.initForm = initForm;
		this.isSpecial = isSpecial;
	}
}
