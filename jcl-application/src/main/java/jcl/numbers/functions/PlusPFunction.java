/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.RealStruct;
import jcl.symbols.BooleanStructs;
import org.springframework.stereotype.Component;

@Component
public final class PlusPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "PLUSP";
	private static final String REAL_ARGUMENT = "REAL";

	public PlusPFunction() {
		super("Returns true if real is greater than zero; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT));
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		final boolean result = real.plusp();
		return BooleanStructs.toLispBoolean(result);
	}
}
