/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class GC extends ExtensionsBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GC";

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

		if (log.isDebugEnabled()) {
			final long bytesRemoved = freeMemoryAfter - freeMemoryBefore;
			log.debug("; {} bytes of garbage removed, current free memory is {} bytes.", bytesRemoved, freeMemoryAfter);
		}

		return TStruct.INSTANCE;
	}
}
