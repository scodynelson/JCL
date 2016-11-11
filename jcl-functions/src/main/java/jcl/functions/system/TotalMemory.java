/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import java.math.BigInteger;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
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
		return LispStructFactory.toInteger(BigInteger.valueOf(totalMemory));
	}
}
