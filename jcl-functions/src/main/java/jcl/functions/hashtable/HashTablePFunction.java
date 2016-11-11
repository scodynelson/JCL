/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.HashTableStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class HashTablePFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "HASH-TABLE-P";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public HashTablePFunction() {
		super("Returns true if object is of type hash-table; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return LispStructFactory.toBoolean(object instanceof HashTableStruct);
	}
}
