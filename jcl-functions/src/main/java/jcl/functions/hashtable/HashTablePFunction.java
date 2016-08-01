/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.hashtable.HashTableStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class HashTablePFunction extends CommonLispBuiltInFunctionStruct {

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
		return BooleanStruct.toLispBoolean(object instanceof HashTableStructImpl);
	}
}
