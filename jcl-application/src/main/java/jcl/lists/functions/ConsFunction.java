/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ConsFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "CONS";
	private static final String OBJECT_1_ARGUMENT = "OBJECT1";
	private static final String OBJECT_2_ARGUMENT = "OBJECT2";

	public ConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_1_ARGUMENT)
		                .requiredParameter(OBJECT_2_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object1 = arguments.getRequiredArgument(OBJECT_1_ARGUMENT);
		final LispStruct object2 = arguments.getRequiredArgument(OBJECT_2_ARGUMENT);
		return new ConsStruct(object1, object2);
	}
}
