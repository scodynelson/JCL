/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class NullFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "NULL";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public NullFunction() {
		super("Returns T if object is the empty list; otherwise, returns NIL.",
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
