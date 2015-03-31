/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.BodyBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class BodyParseResult extends ParseResult {

	private final BodyBinding bodyBinding;

	BodyParseResult(final LispStruct currentElement, final int currentPosition, final BodyBinding bodyBinding) {
		super(currentElement, currentPosition);
		this.bodyBinding = bodyBinding;
	}

	BodyBinding getBodyBinding() {
		return bodyBinding;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bodyBinding)
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
		final BodyParseResult rhs = (BodyParseResult) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bodyBinding, rhs.bodyBinding)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bodyBinding)
		                                                                .toString();
	}
}
