/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.array;

import jcl.lang.BooleanStructs;
import jcl.lang.LispStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class StringPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "STRINGP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public StringPFunction() {
		super("",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return BooleanStructs.toLispBoolean(object instanceof StringStruct);
	}
}