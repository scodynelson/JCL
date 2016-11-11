/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class NotFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "NOT";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public NotFunction() {
		super("Returns T if x is false; otherwise, returns NIL.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return LispStructFactory.toBoolean(NILStruct.INSTANCE.equals(object));
	}
}
