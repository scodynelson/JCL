/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.SystemBuiltInFunctionStructBase;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolFunctionFunction extends SystemBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SET-SYMBOL-FUNCTION";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String FUNCTION_ARGUMENT = "FUNCTION";

	public SetSymbolFunctionFunction() {
		super("Sets the function value of the provided symbol to the provided function value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(FUNCTION_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final FunctionStruct function = arguments.getRequiredArgument(FUNCTION_ARGUMENT, FunctionStruct.class);

		symbol.setFunction(function);
		return function;
	}
}
