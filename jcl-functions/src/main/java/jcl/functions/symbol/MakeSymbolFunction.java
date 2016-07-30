/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.lang.LispStruct;
import jcl.lang.array.StringStructImpl;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MakeSymbolFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-SYMBOL";
	private static final String NAME_ARGUMENT = "NAME";

	public MakeSymbolFunction() {
		super("Creates and returns a fresh, uninterned symbol whose name is the given name.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NAME_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StringStructImpl name = arguments.getRequiredArgument(NAME_ARGUMENT, StringStructImpl.class);
		return name.asSymbol().get();
	}
}
