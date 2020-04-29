/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.system;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import lombok.extern.log4j.Log4j2;

@Log4j2
public final class GC extends BuiltInFunctionStructImpl {

	public GC() {
		super("Manually invokes the Java runtime garbage collection.",
		      CommonLispSymbols.GC.getName(),
		      Parameters.forFunction(CommonLispSymbols.GC.getName())
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.GC;
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
