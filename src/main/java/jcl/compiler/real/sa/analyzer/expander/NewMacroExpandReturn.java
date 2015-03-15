/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.expander;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
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
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).setExcludeFieldNames("parent").toString();
	}
}
