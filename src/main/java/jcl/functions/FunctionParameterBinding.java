/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class FunctionParameterBinding {

	private SymbolStruct parameterSymbol;

	private LispStruct parameterValue;

	private boolean isSpecial;

	public FunctionParameterBinding(final SymbolStruct parameterSymbol, final LispStruct parameterValue,
	                                final boolean isSpecial) {
		this.parameterSymbol = parameterSymbol;
		this.parameterValue = parameterValue;
		this.isSpecial = isSpecial;
	}

	public SymbolStruct getParameterSymbol() {
		return parameterSymbol;
	}

	public LispStruct getParameterValue() {
		return parameterValue;
	}

	public boolean isSpecial() {
		return isSpecial;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(parameterSymbol)
		                            .append(parameterValue)
		                            .append(isSpecial)
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
		final FunctionParameterBinding rhs = (FunctionParameterBinding) obj;
		return new EqualsBuilder().append(parameterSymbol, rhs.parameterSymbol)
		                          .append(parameterValue, rhs.parameterValue)
		                          .append(isSpecial, rhs.isSpecial)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(parameterSymbol)
		                                                                .append(parameterValue)
		                                                                .append(isSpecial)
		                                                                .toString();
	}
}
