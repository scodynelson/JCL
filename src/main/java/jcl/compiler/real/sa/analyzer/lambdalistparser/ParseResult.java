/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

class ParseResult {

	private final LispStruct currentElement;

	private final int currentPosition;

	ParseResult(final LispStruct currentElement, final int currentPosition) {
		this.currentElement = currentElement;
		this.currentPosition = currentPosition;
	}

	LispStruct getCurrentElement() {
		return currentElement;
	}

	int getCurrentPosition() {
		return currentPosition;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(currentElement)
		                            .append(currentPosition)
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
		final ParseResult rhs = (ParseResult) obj;
		return new EqualsBuilder().append(currentElement, rhs.currentElement)
		                          .append(currentPosition, rhs.currentPosition)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(currentElement)
		                                                                .append(currentPosition)
		                                                                .toString();
	}
}
