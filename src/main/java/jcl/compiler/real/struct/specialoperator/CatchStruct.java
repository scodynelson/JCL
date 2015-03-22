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

public class CatchStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -1022768814372160089L;

	private final LispStruct catchTag;

	private final PrognStruct forms;

	public CatchStruct(final LispStruct catchTag, final PrognStruct forms) {
		this.catchTag = catchTag;
		this.forms = forms;
	}

	public LispStruct getCatchTag() {
		return catchTag;
	}

	public PrognStruct getForms() {
		return forms;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(catchTag)
		                            .append(forms)
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
		final CatchStruct rhs = (CatchStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(catchTag, rhs.catchTag)
		                          .append(forms, rhs.forms)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(catchTag)
		                                                                .append(forms)
		                                                                .toString();
	}
}
