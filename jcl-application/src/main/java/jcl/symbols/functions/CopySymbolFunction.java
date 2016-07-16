/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class CopySymbolFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COPY-SYMBOL";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String COPY_PROPERTIES_ARGUMENT = "COPY-PROPERTIES";

	public CopySymbolFunction() {
		super("Returns a fresh, uninterned symbol, the name of which is equal to and possibly the same as the name of the given symbol.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .optionalParameter(COPY_PROPERTIES_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final boolean copyProperties = arguments.getOptionalArgument(COPY_PROPERTIES_ARGUMENT, BooleanStruct.class).booleanValue();
		return symbol.copySymbol(copyProperties);
	}
}
