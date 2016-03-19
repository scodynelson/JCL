/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.RealStruct;
import jcl.symbols.BooleanStructs;
import org.springframework.stereotype.Component;

@Component
public final class GreaterThanFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = ">";
	private static final String REAL_ARGUMENT = "REAL";

	public GreaterThanFunction() {
		super("Returns true if the numbers are in monotonically decreasing order; otherwise returns false.",
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

		final boolean result = RealStruct.isGreaterThan(real, reals);
		return BooleanStructs.toLispBoolean(result);
	}
}
