/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.math.BigDecimal;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.FloatStruct;
import jcl.numbers.RealStruct;
import org.springframework.stereotype.Component;

@Component
public final class FloatFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FLOAT";
	private static final String REAL_ARGUMENT = "REAL";
	private static final String PROTOTYPE_ARGUMENT = "PROTOTYPE";

	public FloatFunction() {
		super("Converts a real number to a float.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(REAL_ARGUMENT)
		                .optionalParameter(PROTOTYPE_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final RealStruct real = arguments.getRequiredArgument(REAL_ARGUMENT, RealStruct.class);
		if (real instanceof FloatStruct) {
			return real;
		}

		final BigDecimal bigDecimal = real.bigDecimalValue();
		return FloatStruct.valueOf(bigDecimal);
	}
}
