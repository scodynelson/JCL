/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.ListStruct;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class NthFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "NTH";
	private static final String INDEX_ARGUMENT = "INDEX";
	private static final String LIST_ARGUMENT = "LIST";

	public NthFunction() {
		super("Locates the nth element of list, where the car of the list is the ``zeroth'' element.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INDEX_ARGUMENT)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct index = arguments.getRequiredArgument(INDEX_ARGUMENT, IntegerStruct.class);
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);

		final long indexValue = index.longValue();
		return list.nth(indexValue);
	}
}
