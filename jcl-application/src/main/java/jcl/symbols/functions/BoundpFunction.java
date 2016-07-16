/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStructs;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class BoundpFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "BOUNDP";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";

	public BoundpFunction() {
		super("Returns true if symbol is bound; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final boolean hasValue = symbol.hasValue();
		return BooleanStructs.toLispBoolean(hasValue);
	}
}
