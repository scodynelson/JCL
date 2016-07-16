/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.ExtensionsBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class FreeMemory extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FREE-MEMORY";

	public FreeMemory() {
		super("Returns the current free runtime memory usage.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final long freeMemory = Runtime.getRuntime().freeMemory();
		return IntegerStruct.valueOf(BigInteger.valueOf(freeMemory));
	}
}
