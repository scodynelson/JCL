/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ListStarFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LIST*";

	public ListStarFunction() {
		super("Returns a list containing the supplied objects where the last argument becomes the cdr of the last cons constructed.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> objects = arguments.getRestArgument();
		return ListStruct.buildDottedList(objects);
	}
}
