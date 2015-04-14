/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

/*
 * GC.java
 *
 * Created on April 23, 2007, 11:51 AM
 */
package jcl.system.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public final class GC extends FunctionStruct {

	public static final SymbolStruct<?> GC = new SymbolStruct<>("GC", GlobalPackageStruct.EXTENSIONS);

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
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();
		final RestBinding restBinding = null;
		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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
