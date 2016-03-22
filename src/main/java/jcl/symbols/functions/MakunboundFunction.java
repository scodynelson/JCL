/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakunboundFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKUNBOUND";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";

	public MakunboundFunction() {
		super("Makes the symbol be unbound, regardless of whether it was previously bound.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		symbol.setValue(null);
		return symbol;
	}
}
