/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class BoundpFunction extends CommonLispBuiltInFunctionStructBase {

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
		return BooleanStruct.toLispBoolean(hasValue);
	}
}
