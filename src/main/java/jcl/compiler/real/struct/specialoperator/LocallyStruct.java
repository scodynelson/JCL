/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.LocallyEnvironment;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LocallyStruct implements LispStruct {

	private static final long serialVersionUID = 3549306656634788482L;

	private final List<LispStruct> forms;

	private final LocallyEnvironment locallyEnvironment;

	public LocallyStruct(final List<LispStruct> forms, final LocallyEnvironment locallyEnvironment) {
		this.forms = forms;
		this.locallyEnvironment = locallyEnvironment;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	public LocallyEnvironment getLocallyEnvironment() {
		return locallyEnvironment;
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
