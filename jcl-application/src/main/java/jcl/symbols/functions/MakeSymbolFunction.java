/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
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
		final StringStruct name = arguments.getRequiredArgument(NAME_ARGUMENT, StringStruct.class);
		return name.asSymbol().get();
	}
}
