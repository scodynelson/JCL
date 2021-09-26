/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.ConsStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;

public final class SetSymbolSetfFunctionFunction extends BuiltInFunctionStructImpl {

	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String FUNCTION_ARGUMENT = "FUNCTION";

	public SetSymbolSetfFunctionFunction() {
		super("Sets the SETF function value of the provided symbol to the provided function value.",
		      CommonLispSymbols.SET_SYMBOL_SETF_FUNCTION.getName(),
		      Parameters.forFunction(CommonLispSymbols.SET_SYMBOL_SETF_FUNCTION.getName())
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(FUNCTION_ARGUMENT)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.SET_SYMBOL_SETF_FUNCTION;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ConsStruct consStruct = arguments.getRequiredArgument(SYMBOL_ARGUMENT, ConsStruct.class);
		final FunctionStruct function = arguments.getRequiredArgument(FUNCTION_ARGUMENT, FunctionStruct.class);

		final SymbolStruct symbol = (SymbolStruct) consStruct.nth(FixnumStruct.ONE);
		symbol.setProp(CommonLispSymbols.SETF_DEFINITION, function);
		return function;
	}
}
