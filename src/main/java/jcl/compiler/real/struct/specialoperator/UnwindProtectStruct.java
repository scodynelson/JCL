/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class UnwindProtectStruct implements LispStruct {

	private static final long serialVersionUID = 2849602976511423223L;

	private final LispStruct protectedForm;

	private final List<LispStruct> cleanupForms;

	public UnwindProtectStruct(final LispStruct protectedForm, final List<LispStruct> cleanupForms) {
		this.protectedForm = protectedForm;
		this.cleanupForms = cleanupForms;
	}

	public LispStruct getProtectedForm() {
		return protectedForm;
	}

	public List<LispStruct> getCleanupForms() {
		return cleanupForms;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
