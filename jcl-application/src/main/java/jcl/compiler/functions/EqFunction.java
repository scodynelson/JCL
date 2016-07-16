/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.function.EquatorFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class EqFunction extends EquatorFunctionStruct {

	private static final String FUNCTION_NAME = "EQL";
	private static final String OBJECT1_ARGUMENT = "OBJECT-1";
	private static final String OBJECT2_ARGUMENT = "OBJECT-2";

	public EqFunction() {
		super("Returns true if its arguments are the same, identical object; otherwise, returns false.",
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
		return BooleanStructs.toLispBoolean(object1.eq(object2));
	}
}
