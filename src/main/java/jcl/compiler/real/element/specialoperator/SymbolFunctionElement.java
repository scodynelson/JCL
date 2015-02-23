/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.SymbolElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolFunctionElement implements FunctionElement {

	private static final long serialVersionUID = 1450935885516226944L;

	private final SymbolElement functionSymbol;

	public SymbolFunctionElement(final SymbolElement functionSymbol) {
		this.functionSymbol = functionSymbol;
	}

	public SymbolElement getFunctionSymbol() {
		return functionSymbol;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
