/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class SymbolFunctionCallStruct extends CompilerSpecialOperatorStruct {

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
	public String toString() {

		final StringBuilder builder = new StringBuilder("(");

		final SymbolStruct functionSymbol = symbolCompilerFunction.getFunctionSymbol();
		final String functionSymbolPrinted = functionSymbol.toString();
		builder.append(functionSymbolPrinted);
		builder.append(' ');

		final String printedArguments =
				arguments.stream()
				         .map(Object::toString)
				         .collect(Collectors.joining(" "));
		builder.append(printedArguments);
		builder.append(')');

		return builder.toString();
	}
}
