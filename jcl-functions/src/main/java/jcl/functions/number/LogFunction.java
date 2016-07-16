/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.NumberStruct;
import org.springframework.stereotype.Component;

@Component
public final class LogFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOG";
	private static final String NUMBER_ARGUMENT = "NUMBER";
	private static final String BASE_ARGUMENT = "BASE";

	public LogFunction() {
		super("Returns the logarithm of number in base base. If base is not supplied its value is e, the base of the natural logarithms.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NUMBER_ARGUMENT)
		                .optionalParameter(BASE_ARGUMENT).withInitialValue(null)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		if (arguments.hasOptionalArgument(BASE_ARGUMENT)) {
			final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
			final NumberStruct base = arguments.getRequiredArgument(BASE_ARGUMENT, NumberStruct.class);
			return number.log(base);
		} else {
			final NumberStruct number = arguments.getRequiredArgument(NUMBER_ARGUMENT, NumberStruct.class);
			return number.log();
		}
	}
}
