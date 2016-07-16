/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.function.ExtensionsBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public final class GC extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "GC";

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(GC.class);

	public GC() {
		super("Manually invokes the Java runtime garbage collection.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final Arguments arguments) {
		final long freeMemoryBefore = Runtime.getRuntime().freeMemory();
		Runtime.getRuntime().gc();
		final long freeMemoryAfter = Runtime.getRuntime().freeMemory();
		LOGGER.debug("; {} bytes of garbage removed, current free memory is {} bytes.", freeMemoryAfter - freeMemoryBefore, freeMemoryAfter);

		return TStruct.INSTANCE;
	}
}
