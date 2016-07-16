/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class LogAndFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOGAND";

	public LogAndFunction() {
		super("Returns the AND of the integers.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<IntegerStruct> integers = arguments.getRestArgument(IntegerStruct.class);
		return IntegerStruct.logAnd(integers);
	}
}
