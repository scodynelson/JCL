/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class EnvironmentParseResult extends ParseResult {

	private final Environment environment;

	EnvironmentParseResult(final LispStruct currentElement, final int currentPosition, final Environment environment) {
		super(currentElement, currentPosition);
		this.environment = environment;
	}

	Environment getEnvironment() {
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
		final EnvironmentParseResult rhs = (EnvironmentParseResult) obj;
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
