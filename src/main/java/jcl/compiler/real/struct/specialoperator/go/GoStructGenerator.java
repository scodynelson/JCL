/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import java.io.Serializable;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class GoStructGenerator<T extends LispStruct> implements Serializable {

	private static final long serialVersionUID = -5463110914990614222L;

	protected GoStructGenerator() {
	}

	public abstract GoStruct<T> generateGoElement(final T tag);

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
