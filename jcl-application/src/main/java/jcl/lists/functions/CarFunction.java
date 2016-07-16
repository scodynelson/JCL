/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class CarFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "CAR";
	private static final String LIST_ARGUMENT = "LIST";

	public CarFunction() {
		super("Gets the car of the provided list.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		return list.getCar();
	}
}
