/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.RealStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class LessThanOrEqualToFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "<=";
	private static final String REAL_ARGUMENT = "REAL";

	public LessThanOrEqualToFunction() {
		super("Returns true if the numbers are in monotonically nondecreasing order; otherwise returns false.",
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

		final RealStruct[] realsToCompare = new RealStruct[reals.size() + 1];
		realsToCompare[0] = real;
		for (int i = 1; i < realsToCompare.length; i++) {
			final RealStruct realToCompare = reals.get(i - 1);
			realsToCompare[i] = realToCompare;
		}

		final boolean lessThanOrEqualTo = RealStruct.isLessThanOrEqualTo(realsToCompare);
		return BooleanStruct.toLispBoolean(lessThanOrEqualTo);
	}
}
