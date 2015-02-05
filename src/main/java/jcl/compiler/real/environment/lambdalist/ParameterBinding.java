/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.lambdalist;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.Binding;
import jcl.compiler.real.environment.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ParameterBinding extends Binding<ParameterAllocation> {

	private static final long serialVersionUID = 4128878993186537174L;

	private final LispStruct initForm;

	protected ParameterBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final Scope scope,
	                        final LispType type, final LispStruct initForm) {
		super(symbolStruct, allocation, scope, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
