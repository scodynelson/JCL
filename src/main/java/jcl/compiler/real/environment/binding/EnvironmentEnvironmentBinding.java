/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.EnvironmentAllocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentEnvironmentBinding extends EnvironmentBinding<EnvironmentAllocation> {

	private static final long serialVersionUID = 1487409353000374736L;

	private Environment environment;

	public EnvironmentEnvironmentBinding(final SymbolStruct<?> symbolStruct, final EnvironmentAllocation allocation, final LispType type,
	                                     final Environment environment) {
		super(symbolStruct, allocation, type);
		this.environment = environment;
	}

	public Environment getEnvironment() {
		return environment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(environment)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final EnvironmentEnvironmentBinding rhs = (EnvironmentEnvironmentBinding) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(environment, rhs.environment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(environment)
		                                                                .toString();
	}
}
