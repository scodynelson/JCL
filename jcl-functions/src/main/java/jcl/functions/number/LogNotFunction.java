/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class LogNotFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LOGNOT";
	private static final String INTEGER_ARGUMENT = "INTEGER";

	public LogNotFunction() {
		super("Returns the COMPLEMENT of integer.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INTEGER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct integer = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStruct.class);
		return integer.logNot();
	}
}
