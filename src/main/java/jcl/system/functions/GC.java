/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public final class GC extends FunctionStruct {

	public static final SymbolStruct<?> GC = GlobalPackageStruct.EXTENSIONS.intern("GC").getSymbol();

	private static final long serialVersionUID = 1273370280152802930L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(GC.class);

	private GC() {
		super("Manually invokes the Java runtime garbage collection.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		GC.setFunction(this);
		GlobalPackageStruct.EXTENSIONS.export(GC);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		return new OrdinaryLambdaList.Builder().build();
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final long freeMemoryBefore = Runtime.getRuntime().freeMemory();
		Runtime.getRuntime().gc();
		final long freeMemoryAfter = Runtime.getRuntime().freeMemory();
		LOGGER.debug("; " + (freeMemoryAfter - freeMemoryBefore) + " bytes of garbage removed, current free memory is " + freeMemoryAfter + " bytes.");

		return TStruct.INSTANCE;
	}
}
