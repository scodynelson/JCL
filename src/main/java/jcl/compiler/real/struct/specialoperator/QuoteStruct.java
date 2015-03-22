/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class QuoteStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 2854755653951600124L;

	private final LispStruct object;

	public QuoteStruct(final LispStruct object) {
		this.object = object;
	}

	public LispStruct getObject() {
		return object;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(object)
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
		final QuoteStruct rhs = (QuoteStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(object, rhs.object)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(object)
		                                                                .toString();
	}
}
