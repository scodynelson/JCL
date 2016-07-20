/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class GreaterThanOrEqualToFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = ">=";
	private static final String REAL_ARGUMENT = "REAL";

	public GreaterThanOrEqualToFunction() {
		super("Returns true if the numbers are in monotonically nonincreasing order; otherwise returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		final List<RealStruct> reals = arguments.getRestArgument(RealStruct.class);

		final boolean result = RealStruct.isGreaterThanOrEqualTo(real, reals);
		return BooleanStructs.toLispBoolean(result);
	}
}