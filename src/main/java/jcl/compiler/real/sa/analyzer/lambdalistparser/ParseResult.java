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

	ParseResult(final LispStruct currentElement) {
		this.currentElement = currentElement;
	}

	LispStruct getCurrentElement() {
		return currentElement;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(currentElement)
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
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(currentElement)
		                                                                .toString();
	}
}
