/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.SystemBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class BindSymbolFunctionFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "BIND-SYMBOL-FUNCTION";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String FUNCTION_ARGUMENT = "FUNCTION";

	public BindSymbolFunctionFunction() {
		super("Binds the function value of the provided symbol to the provided function value.",
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

		symbol.bindFunction(function);
		return new ValuesStruct();
	}
}
