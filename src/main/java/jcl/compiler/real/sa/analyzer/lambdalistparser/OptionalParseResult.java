/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class OptionalParseResult extends ParseResult {

	private final List<OptionalBinding> optionalBindings;

	OptionalParseResult(final LispStruct currentElement, final int currentPosition, final List<OptionalBinding> optionalBindings) {
		super(currentElement, currentPosition);
		this.optionalBindings = optionalBindings;
	}

	List<OptionalBinding> getOptionalBindings() {
		return optionalBindings;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(optionalBindings)
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
		final OptionalParseResult rhs = (OptionalParseResult) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(optionalBindings, rhs.optionalBindings)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(optionalBindings)
		                                                                .toString();
	}
}
