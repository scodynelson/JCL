/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.number;

import java.util.List;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class LogXorFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "LOGXOR";

	public LogXorFunction() {
		super("Returns the EXCLUSIVE-OR of the integers.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<IntegerStruct> integers = arguments.getRestArgument(IntegerStruct.class);
		return IntegerStruct.logXor(integers);
	}
}
