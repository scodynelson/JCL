/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

/*
 * GC.java
 *
 * Created on April 23, 2007, 11:51 AM
 */
package jcl.system.functions;

import java.math.BigInteger;
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
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class MaxMemory extends FunctionStruct {

	public static final SymbolStruct<?> MAX_MEMORY = new SymbolStruct<>("MAX-MEMORY", GlobalPackageStruct.EXTENSIONS);

	private static final long serialVersionUID = -8202963218833948816L;

	private MaxMemory() {
		super("Returns the current max runtime memory usage.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MAX_MEMORY.setFunction(this);
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

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final long maxMemory = Runtime.getRuntime().maxMemory();
		return new IntegerStruct(BigInteger.valueOf(maxMemory));
	}
}
