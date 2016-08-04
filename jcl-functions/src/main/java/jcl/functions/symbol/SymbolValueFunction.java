/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SymbolValueFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SYMBOL-VALUE";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";

	public SymbolValueFunction() {
		super("Gets the value of the provided symbol.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		);
	}


	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStructImpl symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStructImpl.class);
		return symbol.getValue();
	}
}
