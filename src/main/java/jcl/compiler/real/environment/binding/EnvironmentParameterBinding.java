/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentParameterBinding extends EnvironmentBinding<ParameterAllocation> {

	private static final long serialVersionUID = -4911545990664188285L;

	private final LispStruct initForm;

	public EnvironmentParameterBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispType type,
	                                   final LispStruct initForm) {
		super(symbolStruct, allocation, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
