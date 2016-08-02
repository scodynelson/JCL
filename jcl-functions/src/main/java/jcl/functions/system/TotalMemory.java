/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.function.ExtensionsBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStructImpl;
import org.springframework.stereotype.Component;

@Component
public final class TotalMemory extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "TOTAL-MEMORY";

	public TotalMemory() {
		super("Returns the current total runtime memory usage.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final long totalMemory = Runtime.getRuntime().totalMemory();
		return IntegerStructImpl.valueOf(BigInteger.valueOf(totalMemory));
	}
}
