/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class LogBitPFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LOGBITP";
	private static final String INDEX_ARGUMENT = "INDEX";
	private static final String INTEGER_ARGUMENT = "INTEGER";

	public LogBitPFunction() {
		super("Returns true if the bit in integer whose index is index (that is, its weight is 2^index) is a one-bit; otherwise it is false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INDEX_ARGUMENT)
		                .requiredParameter(INTEGER_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct integer = arguments.getRequiredArgument(INDEX_ARGUMENT, IntegerStruct.class);
		final IntegerStruct index = arguments.getRequiredArgument(INTEGER_ARGUMENT, IntegerStruct.class);

		final boolean result = integer.logBitP(index);
		return LispStructFactory.toBoolean(result);
	}
}
