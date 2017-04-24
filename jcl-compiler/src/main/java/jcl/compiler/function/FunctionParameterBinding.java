/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

public class FunctionParameterBinding {

	private final SymbolStruct parameterSymbol;

	private final LispStruct parameterValue;

	private final boolean isSpecial;

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
}
