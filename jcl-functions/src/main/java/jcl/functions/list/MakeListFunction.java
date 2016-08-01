/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.list;

import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeListFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-LIST";
	private static final String SIZE_ARGUMENT = "SIZE";

	public MakeListFunction() {
		super("Returns a list of length given by size, each of the elements of which is initial-element.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SIZE_ARGUMENT)
		                .keyParameter(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final IntegerStruct size = arguments.getRequiredArgument(SIZE_ARGUMENT, IntegerStruct.class);
		final LispStruct initialElement = arguments.getKeyArgument(CommonLispSymbols.INITIAL_ELEMENT_KEYWORD);
		return ListStruct.makeList(size.longValue(), initialElement);
	}
}
