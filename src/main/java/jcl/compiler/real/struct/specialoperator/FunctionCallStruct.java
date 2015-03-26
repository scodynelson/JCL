/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class FunctionCallStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 2676444242188589421L;

	private final boolean isRecursiveCall;

	private final SymbolStruct<?> functionSymbol;

	private final List<LispStruct> arguments;

	public FunctionCallStruct(final boolean isRecursiveCall, final SymbolStruct<?> functionSymbol, final List<LispStruct> arguments) {
		this.isRecursiveCall = isRecursiveCall;
		this.functionSymbol = functionSymbol;
		this.arguments = arguments;
	}

	public boolean isRecursiveCall() {
		return isRecursiveCall;
	}

	public SymbolStruct<?> getFunctionSymbol() {
		return functionSymbol;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(isRecursiveCall)
		                            .append(functionSymbol)
		                            .append(arguments)
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
		final FunctionCallStruct rhs = (FunctionCallStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(isRecursiveCall, rhs.isRecursiveCall)
		                          .append(functionSymbol, rhs.functionSymbol)
		                          .append(arguments, rhs.arguments)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(isRecursiveCall)
		                                                                .append(functionSymbol)
		                                                                .append(arguments)
		                                                                .toString();
	}
}
