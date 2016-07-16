/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class VectorFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "VECTOR";

	public VectorFunction() {
		super("Returns the absolute value of number.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final List<LispStruct> objects = arguments.getRestArgument();
		return new VectorStruct<>(objects);
	}
}
