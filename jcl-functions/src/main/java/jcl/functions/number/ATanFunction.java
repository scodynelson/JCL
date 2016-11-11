/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ATanFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "ATAN";
	private static final String NUMBER1_ARGUMENT = "NUMBER1";
	private static final String NUMBER2_ARGUMENT = "NUMBER2";

	public ATanFunction() {
		super("Returns the arc-tangent of number. If both number1 and number2 are supplied, the result is the arc-tangent of number1/number2.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER1_ARGUMENT)
		                .optionalParameter(NUMBER2_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		if (arguments.hasOptionalArgument(NUMBER2_ARGUMENT)) {
			final RealStruct number1 = arguments.getRequiredArgument(NUMBER1_ARGUMENT, RealStruct.class);
			final RealStruct number2 = arguments.getOptionalArgument(NUMBER2_ARGUMENT, RealStruct.class);
			return number1.atan(number2);
		} else {
			final NumberStruct number = arguments.getRequiredArgument(NUMBER1_ARGUMENT, NumberStruct.class);
			return number.atan();
		}
	}
}
