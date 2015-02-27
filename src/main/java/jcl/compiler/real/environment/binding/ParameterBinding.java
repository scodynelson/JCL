/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ParameterBinding extends Binding<ParameterAllocation> {

	private static final long serialVersionUID = 4128878993186537174L;

	private final SimpleElement initForm;

	protected ParameterBinding(final SymbolElement symbolStruct, final ParameterAllocation allocation, final LispType type,
	                           final SimpleElement initForm) {
		super(symbolStruct, allocation, type);
		this.initForm = initForm;
	}

	public SimpleElement getInitForm() {
		return initForm;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
