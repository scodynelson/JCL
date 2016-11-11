/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class CopySymbolFunction extends CommonLispBuiltInFunctionStructBase {

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
