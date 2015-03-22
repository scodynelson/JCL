/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import java.io.Serializable;

import jcl.LispType;
import jcl.compiler.real.environment.allocation.Allocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class Binding<A extends Allocation> implements Serializable {

	private static final long serialVersionUID = 5776546981120800982L;

	private final SymbolStruct<?> symbolStruct;

	private final A allocation;

	private final LispType type;

	protected Binding(final SymbolStruct<?> symbolStruct, final A allocation, final LispType type) {
		this.symbolStruct = symbolStruct;
		this.allocation = allocation;
		this.type = type;
	}

	public SymbolStruct<?> getSymbolStruct() {
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
		return new HashCodeBuilder().append(symbolStruct)
		                            .append(allocation)
		                            .append(type)
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
		final Binding<?> rhs = (Binding) obj;
		return new EqualsBuilder().append(symbolStruct, rhs.symbolStruct)
		                          .append(allocation, rhs.allocation)
		                          .append(type, rhs.type)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbolStruct)
		                                                                .append(allocation)
		                                                                .append(type)
		                                                                .toString();
	}
}
