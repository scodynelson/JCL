/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

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
}
