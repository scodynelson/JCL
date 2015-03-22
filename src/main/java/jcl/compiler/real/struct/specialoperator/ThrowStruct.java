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

public class ThrowStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 935019872276115270L;

	private final LispStruct catchTag;

	private final LispStruct resultForm;

	public ThrowStruct(final LispStruct catchTag, final LispStruct resultForm) {
		this.catchTag = catchTag;
		this.resultForm = resultForm;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public LispStruct getResultForm() {
		return resultForm;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(catchTag)
		                            .append(resultForm)
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
		final ThrowStruct rhs = (ThrowStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(catchTag, rhs.catchTag)
		                          .append(resultForm, rhs.resultForm)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(catchTag)
		                                                                .append(resultForm)
		                                                                .toString();
	}
}
