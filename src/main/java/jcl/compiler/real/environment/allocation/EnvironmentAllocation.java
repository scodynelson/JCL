/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.allocation;

import jcl.compiler.real.environment.Environment;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentAllocation implements Allocation {

	private static final long serialVersionUID = 1294755562180249532L;

	private final Environment environment;

	public EnvironmentAllocation(final Environment environment) {
		this.environment = environment;
	}

	public Environment getEnvironment() {
		return environment;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(environment)
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
		final EnvironmentAllocation rhs = (EnvironmentAllocation) obj;
		return new EqualsBuilder().append(environment, rhs.environment)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(environment)
		                                                                .toString();
	}
}
