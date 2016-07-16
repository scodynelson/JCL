/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class NullFunction extends CommonLispBuiltInFunctionStruct {

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
		return BooleanStructs.toLispBoolean(NILStruct.INSTANCE.equals(object));
	}
}
