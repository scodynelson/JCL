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
		return new IntIntegerStruct(BigInteger.valueOf(totalMemory));
	}
}
