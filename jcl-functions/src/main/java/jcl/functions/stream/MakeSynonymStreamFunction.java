/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MakeSynonymStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-SYNONYM-STREAM";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";

	public MakeSynonymStreamFunction() {
		super("Returns a synonym stream whose synonym stream symbol is symbol.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStructImpl symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStructImpl.class);
		return LispStructFactory.toSynonymStream(symbol);
	}
}
