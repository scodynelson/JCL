/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class NewMacroExpandReturn {

	private final LispStruct expandedForm;

	private final boolean wasExpanded;

	public NewMacroExpandReturn(final LispStruct expandedForm, final boolean wasExpanded) {
		this.expandedForm = expandedForm;
		this.wasExpanded = wasExpanded;
	}

	public LispStruct getExpandedForm() {
		return expandedForm;
	}

	public boolean wasExpanded() {
		return wasExpanded;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(expandedForm)
		                            .append(wasExpanded)
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
		final NewMacroExpandReturn rhs = (NewMacroExpandReturn) obj;
		return new EqualsBuilder().append(expandedForm, rhs.expandedForm)
		                          .append(wasExpanded, rhs.wasExpanded)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(expandedForm)
		                                                                .append(wasExpanded)
		                                                                .toString();
	}
}
