/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class RempropFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "REMPROP";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String INDICATOR_ARGUMENT = "INDICATOR";

	public RempropFunction() {
		super("Removes from the property list of symbol a property[1] with a property indicator identical to indicator.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(INDICATOR_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final LispStruct indicator = arguments.getRequiredArgument(INDICATOR_ARGUMENT);
		final boolean wasRemoved = symbol.removeProperty(indicator);

		return BooleanStructs.toLispBoolean(wasRemoved);
	}
}
