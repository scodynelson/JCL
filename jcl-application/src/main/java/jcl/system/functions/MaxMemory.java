/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.function.ExtensionsBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class MaxMemory extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAX-MEMORY";

	public MaxMemory() {
		super("Returns the current max runtime memory usage.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final long maxMemory = Runtime.getRuntime().maxMemory();
		return IntegerStruct.valueOf(BigInteger.valueOf(maxMemory));
	}
}
