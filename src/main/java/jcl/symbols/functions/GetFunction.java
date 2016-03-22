/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class GetFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "GET";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String INDICATOR_ARGUMENT = "INDICATOR";
	private static final String DEFAULT_ARGUMENT = "DEFAULT";

	public GetFunction() {
		super("Finds a property on the property list of symbol whose property indicator is identical to indicator, and returns its corresponding property value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(INDICATOR_ARGUMENT)
		                .optionalParameter(DEFAULT_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final LispStruct indicator = arguments.getRequiredArgument(INDICATOR_ARGUMENT);
		final LispStruct defaultValue = arguments.getOptionalArgument(DEFAULT_ARGUMENT);

		return symbol.getProperty(indicator, defaultValue);
	}
}
