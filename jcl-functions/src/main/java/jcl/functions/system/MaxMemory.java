/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
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
		return LispStructFactory.toInteger(BigInteger.valueOf(maxMemory));
	}
}
