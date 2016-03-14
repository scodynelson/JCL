/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.environment.binding.Binding;
import jcl.symbols.SymbolStruct;

public class Parameter extends Binding {

	private LispStruct initForm;

	private Class<? extends LispStruct> initFormClass;

	private final DestructuringLambdaList destructuringForm;

	private final boolean isSpecial;

	protected Parameter(final SymbolStruct var, final DestructuringLambdaList destructuringForm,
	                    final LispType type, final LispStruct initForm, final boolean isSpecial) {
		super(var, type);
		this.destructuringForm = destructuringForm;
		this.initForm = initForm;
		this.isSpecial = isSpecial;
	}

	public DestructuringLambdaList getDestructuringForm() {
		return destructuringForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	public void setInitForm(final LispStruct initForm) {
		this.initForm = initForm;
	}

	public Class<? extends LispStruct> getInitFormClass() {
		return initFormClass;
	}

	public void setInitFormClass(final Class<? extends LispStruct> initFormClass) {
		this.initFormClass = initFormClass;
	}

	public boolean isSpecial() {
		return isSpecial;
	}
}
