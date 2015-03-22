/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolCompilerFunctionStruct implements CompilerFunctionStruct {

	private static final long serialVersionUID = 1450935885516226944L;

	private final SymbolStruct<?> functionSymbol;

	public SymbolCompilerFunctionStruct(final SymbolStruct<?> functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolStruct<?> getFunctionSymbol() {
		return functionSymbol;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(functionSymbol)
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
		final SymbolCompilerFunctionStruct rhs = (SymbolCompilerFunctionStruct) obj;
		return new EqualsBuilder().append(functionSymbol, rhs.functionSymbol)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(functionSymbol)
		                                                                .toString();
	}
}
