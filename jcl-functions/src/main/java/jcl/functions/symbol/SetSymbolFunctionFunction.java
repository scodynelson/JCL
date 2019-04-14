/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class SetSymbolFunctionFunction extends BuiltInFunctionStructImpl {

	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String FUNCTION_ARGUMENT = "FUNCTION";

	public SetSymbolFunctionFunction() {
		super("Sets the function value of the provided symbol to the provided function value.",
		      CommonLispSymbols.SET_SYMBOL_FUNCTION.getName(),
		      Parameters.forFunction(CommonLispSymbols.SET_SYMBOL_FUNCTION.getName())
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(FUNCTION_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.SET_SYMBOL_FUNCTION;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final FunctionStruct function = arguments.getRequiredArgument(FUNCTION_ARGUMENT, FunctionStruct.class);

		symbol.setFunction(function);
		return function;
	}
}
