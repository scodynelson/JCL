/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class LogAndC1Function extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOGANDC1";
	private static final String INTEGER1_ARGUMENT = "INTEGER1";
	private static final String INTEGER2_ARGUMENT = "INTEGER2";

	public LogAndC1Function() {
		super("Returns the AND COMPLEMENT of integer-1 with integer-2.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INTEGER1_ARGUMENT)
		                .requiredParameter(INTEGER2_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct integer1 = arguments.getRequiredArgument(INTEGER1_ARGUMENT, IntegerStruct.class);
		final IntegerStruct integer2 = arguments.getRequiredArgument(INTEGER2_ARGUMENT, IntegerStruct.class);
		return integer1.logAndC1(integer2);
	}
}