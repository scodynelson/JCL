/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import java.io.Serializable;

import jcl.LispType;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public abstract class Binding implements Serializable {

	private static final long serialVersionUID = 5776546981120800982L;

	private final SymbolStruct<?> symbolStruct;

	private final LispType type;

	protected Binding(final SymbolStruct<?> symbolStruct, final LispType type) {
		this.symbolStruct = symbolStruct;
		this.type = type;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public LispType getType() {
		return type;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(symbolStruct)
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
		final Binding rhs = (Binding) obj;
		return new EqualsBuilder().append(symbolStruct, rhs.symbolStruct)
		                          .append(type, rhs.type)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbolStruct)
		                                                                .append(type)
		                                                                .toString();
	}
}
