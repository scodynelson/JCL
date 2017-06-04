/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class EqualpFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "EQUALP";
	private static final String OBJECT1_ARGUMENT = "OBJECT-1";
	private static final String OBJECT2_ARGUMENT = "OBJECT-2";

	public EqualpFunction() {
		super("Returns true if x and y are equal, or if they have components that are of the same type as each other and if those components are equalp.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT1_ARGUMENT)
		                .requiredParameter(OBJECT2_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object1 = arguments.getRequiredArgument(OBJECT1_ARGUMENT);
		final LispStruct object2 = arguments.getRequiredArgument(OBJECT2_ARGUMENT);
		return LispStructFactory.toBoolean(object1.equalp(object2));
	}
}