/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.functioncall;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class FunctionCallStruct implements LispStruct {

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
