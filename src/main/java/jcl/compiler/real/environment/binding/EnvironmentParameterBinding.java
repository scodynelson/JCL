/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentParameterBinding extends EnvironmentBinding<ParameterAllocation> {

	private static final long serialVersionUID = -4911545990664188285L;

	private final Element initForm;

	public EnvironmentParameterBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final Scope scope,
	                                   final LispType type, final Element initForm) {
		super(symbolStruct, allocation, scope, type);
		this.initForm = initForm;
	}

	public Element getInitForm() {
		return initForm;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
