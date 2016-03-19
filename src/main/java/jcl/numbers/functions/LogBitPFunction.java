/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.symbols.BooleanStructs;
import org.springframework.stereotype.Component;

@Component
public final class LogBitPFunction extends CommonLispBuiltInFunctionStruct {

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
		return BooleanStructs.toLispBoolean(result);
	}
}
