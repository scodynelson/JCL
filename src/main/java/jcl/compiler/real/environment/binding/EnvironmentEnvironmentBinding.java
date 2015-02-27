/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentEnvironmentBinding extends EnvironmentBinding<EnvironmentAllocation> {

	private static final long serialVersionUID = 1487409353000374736L;

	private Environment environment;

	public EnvironmentEnvironmentBinding(final SymbolElement symbolStruct, final EnvironmentAllocation allocation, final LispType type,
	                                     final Environment environment) {
		super(symbolStruct, allocation, type);
		this.environment = environment;
	}

	public Environment getEnvironment() {
		return environment;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
