/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.SystemBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolPlistFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SET-SYMBOL-PLIST";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String PLIST_ARGUMENT = "PLIST";

	public SetSymbolPlistFunction() {
		super("Sets the plist value of the provided symbol to the provided plist value.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(PLIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final ListStruct plist = arguments.getRequiredArgument(PLIST_ARGUMENT, ListStruct.class);

		symbol.setProperties(plist);
		return plist;
	}
}
