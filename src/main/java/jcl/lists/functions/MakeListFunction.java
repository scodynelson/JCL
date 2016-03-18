/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
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
		return ListStruct.makeList(size.getBigInteger().longValue(), initialElement);
	}
}
