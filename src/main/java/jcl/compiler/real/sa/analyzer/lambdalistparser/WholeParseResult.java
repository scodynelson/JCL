/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.compiler.real.environment.binding.lambdalist.WholeBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class WholeParseResult extends ParseResult {

	private final WholeBinding wholeBinding;

	WholeParseResult(final int currentPosition, final WholeBinding wholeBinding) {
		super(null, currentPosition);
		this.wholeBinding = wholeBinding;
	}

	WholeBinding getWholeBinding() {
		return wholeBinding;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(wholeBinding)
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
		final WholeParseResult rhs = (WholeParseResult) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(wholeBinding, rhs.wholeBinding)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(wholeBinding)
		                                                                .toString();
	}
}
