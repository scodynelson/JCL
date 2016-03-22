/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.ExtensionsBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntIntegerStruct;
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
		return new IntIntegerStruct(BigInteger.valueOf(maxMemory));
	}
}
