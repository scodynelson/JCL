/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.functioncall;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class FunctionCallStruct extends SpecialOperatorStruct {

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
}
