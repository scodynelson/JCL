/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SetFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SET";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String VALUE_ARGUMENT = "VALUE";

	public SetFunction() {
		super("Sets the value of the provided symbol to the provided value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(VALUE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final LispStruct value = arguments.getRequiredArgument(VALUE_ARGUMENT);

		symbol.setValue(value);
		return value;
	}
}
