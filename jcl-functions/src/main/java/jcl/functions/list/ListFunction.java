/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import java.util.List;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class ListFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LIST";

	public ListFunction() {
		super("Returns a list containing the supplied objects.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> objects = arguments.getRestArgument();
		return ListStruct.toLispList(objects);
	}
}
