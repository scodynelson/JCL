/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SymbolFunctionFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SYMBOL-FUNCTION";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";

	public SymbolFunctionFunction() {
		super("Gets the function value of the provided symbol.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		return symbol.getFunction();
	}
}
