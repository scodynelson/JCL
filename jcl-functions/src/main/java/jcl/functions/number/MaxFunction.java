/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class MaxFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "MAX";
	private static final String REAL_ARGUMENT = "REAL";

	public MaxFunction() {
		super("Returns the real that is greatest (closest to positive infinity)",
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
		return RealStruct.max(real, reals);
	}
}
