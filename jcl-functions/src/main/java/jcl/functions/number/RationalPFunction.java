/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.RationalStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code rationalp}.
 */
@Component
public final class RationalPFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "RATIONALP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public RationalPFunction() {
		super("Returns true if object is of type rational; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return BooleanStruct.toLispBoolean(object instanceof RationalStruct);
	}
}