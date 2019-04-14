/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class TotalMemory extends ExtensionsBuiltInFunctionStructBase {

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
		return IntegerStruct.toLispInteger(totalMemory);
	}
}
