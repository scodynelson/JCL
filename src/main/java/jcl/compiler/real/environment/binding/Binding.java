/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.allocation.Allocation;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;

public abstract class Binding<A extends Allocation> implements Serializable {

	private static final long serialVersionUID = 5776546981120800982L;

	private final SymbolElement symbolStruct;

	private final A allocation;

	private final LispType type;

	protected Binding(final SymbolElement symbolStruct, final A allocation, final LispType type) {
		this.symbolStruct = symbolStruct;
		this.allocation = allocation;
		this.type = type;
	}

	public SymbolElement getSymbolStruct() {
		return symbolStruct;
	}

	public A getAllocation() {
		return allocation;
	}

	public LispType getType() {
		return type;
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
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
