/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class AuxBinding extends ParameterBinding {

	private static final long serialVersionUID = 6424631350711831345L;

	public AuxBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispStruct initForm) {
		super(symbolStruct, allocation, Scope.LEXICAL, T.INSTANCE, initForm);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}