/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import jcl.LispStruct;
import jcl.functions.AbstractExtensionsFunctionStruct;
import jcl.symbols.TStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public final class GC extends AbstractExtensionsFunctionStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(GC.class);

	public GC() {
		super("Manually invokes the Java runtime garbage collection.");
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final long freeMemoryBefore = Runtime.getRuntime().freeMemory();
		Runtime.getRuntime().gc();
		final long freeMemoryAfter = Runtime.getRuntime().freeMemory();
		LOGGER.debug("; {} bytes of garbage removed, current free memory is {} bytes.", freeMemoryAfter - freeMemoryBefore, freeMemoryAfter);

		return TStruct.INSTANCE;
	}

	@Override
	protected String functionName() {
		return "GC";
	}
}
