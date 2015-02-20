/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;

public class FunctionCallElement implements Element {

	private static final long serialVersionUID = 2676444242188589421L;

	private final boolean isRecursiveCall;

	private final SymbolElement<?> functionSymbol;
	private final List<Element> arguments;

	public FunctionCallElement(final boolean isRecursiveCall, final SymbolElement<?> functionSymbol, final List<Element> arguments) {
		this.isRecursiveCall = isRecursiveCall;
		this.functionSymbol = functionSymbol;
		this.arguments = arguments;
	}

	public boolean isRecursiveCall() {
		return isRecursiveCall;
	}

	public SymbolElement<?> getFunctionSymbol() {
		return functionSymbol;
	}

	public List<Element> getArguments() {
		return arguments;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
