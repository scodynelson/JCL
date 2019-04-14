/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class MaxMemory extends ExtensionsBuiltInFunctionStructBase {

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
		return IntegerStruct.toLispInteger(maxMemory);
	}
}
