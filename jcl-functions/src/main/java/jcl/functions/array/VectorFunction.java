/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.array;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.array.VectorStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
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
