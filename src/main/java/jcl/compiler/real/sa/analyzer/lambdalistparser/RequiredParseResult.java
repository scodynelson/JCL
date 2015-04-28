/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class RequiredParseResult extends ParseResult {

	private final List<RequiredBinding> requiredBindings;

	RequiredParseResult(final LispStruct currentElement, final List<RequiredBinding> requiredBindings) {
		super(currentElement);
		this.requiredBindings = requiredBindings;
	}

	List<RequiredBinding> getRequiredBindings() {
		return requiredBindings;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(requiredBindings)
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
		final RequiredParseResult rhs = (RequiredParseResult) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(requiredBindings, rhs.requiredBindings)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(requiredBindings)
		                                                                .toString();
	}
}
