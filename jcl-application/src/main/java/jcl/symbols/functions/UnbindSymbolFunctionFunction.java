/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.SystemBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class UnbindSymbolFunctionFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "UNBIND-SYMBOL-FUNCTION";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";

	public UnbindSymbolFunctionFunction() {
		super("Unbinds the function value of the provided symbol from its current value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		return symbol.unbindFunction();
	}
}
