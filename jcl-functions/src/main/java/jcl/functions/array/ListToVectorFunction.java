/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.array;

import java.util.List;
import java.util.stream.Collectors;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.array.VectorStruct;
import jcl.lang.function.SystemBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ListToVectorFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LIST-TO-VECTOR";
	private static final String LIST_ARGUMENT = "LIST";

	public ListToVectorFunction() {
		super("Creates a fresh simple general vector from the provided list.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final List<LispStruct> collect = list.stream().collect(Collectors.toList());
		return new VectorStruct<>(collect);
	}
}
