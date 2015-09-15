/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolFunctionCallStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 2676444242188589421L;

	private final SymbolCompilerFunctionStruct symbolCompilerFunction;

	private final List<LispStruct> arguments;

	private final boolean isRecursiveCall;

	public SymbolFunctionCallStruct(final SymbolCompilerFunctionStruct symbolCompilerFunction, final List<LispStruct> arguments,
	                                final boolean isRecursiveCall) {
		this.symbolCompilerFunction = symbolCompilerFunction;
		this.arguments = arguments;
		this.isRecursiveCall = isRecursiveCall;
	}

	public SymbolCompilerFunctionStruct getSymbolCompilerFunction() {
		return symbolCompilerFunction;
	}

	public List<LispStruct> getArguments() {
		return arguments;
	}

	public boolean isRecursiveCall() {
		return isRecursiveCall;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(symbolCompilerFunction)
		                            .append(arguments)
		                            .append(isRecursiveCall)
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
		final SymbolFunctionCallStruct rhs = (SymbolFunctionCallStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(symbolCompilerFunction, rhs.symbolCompilerFunction)
		                          .append(arguments, rhs.arguments)
		                          .append(isRecursiveCall, rhs.isRecursiveCall)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbolCompilerFunction)
		                                                                .append(arguments)
		                                                                .append(isRecursiveCall)
		                                                                .toString();
	}
}
