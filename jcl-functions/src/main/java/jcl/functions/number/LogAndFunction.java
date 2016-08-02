/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStructImpl;
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
		final List<IntegerStructImpl> integers = arguments.getRestArgument(IntegerStructImpl.class);
		return IntegerStructImpl.logAnd(integers);
	}
}
