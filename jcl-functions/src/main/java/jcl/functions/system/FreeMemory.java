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
public final class FreeMemory extends ExtensionsBuiltInFunctionStructBase {

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
		return LispStructFactory.toInteger(BigInteger.valueOf(freeMemory));
	}
}
