/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ValuesFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "VALUES";

	public ValuesFunction() {
		super("Returns the objects as multiple values.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> valuesList = arguments.getRestArgument();
		return ValuesStruct.valueOf(valuesList);
	}
}
