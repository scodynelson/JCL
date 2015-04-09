/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays.functions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class VectorFunction extends FunctionStruct {

	public static final SymbolStruct<?> VECTOR = new SymbolStruct<>("VECTOR", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -2957696649653550853L;

	private VectorFunction() {
		super("Creates a fresh simple general vector whose size corresponds to the number of objects.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		VECTOR.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> objectRestArgSymbol = new SymbolStruct<>("OBJECTS", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation objectRestArgArgAllocation = new ParameterAllocation(0);
		final RestBinding restBinding = new RestBinding(objectRestArgSymbol, objectRestArgArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return vector(lispStructs);
	}

	public LispStruct vector(final LispStruct... lispStructs) {
		return new VectorStruct<>(Arrays.asList(lispStructs));
	}
}
