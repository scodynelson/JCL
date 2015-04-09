/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays.functions;

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
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ListToVectorFunction extends FunctionStruct {

	public static final SymbolStruct<?> LIST_TO_VECTOR = new SymbolStruct<>("LIST-TO-VECTOR", GlobalPackageStruct.SYSTEM);

	private static final long serialVersionUID = 7202751615463953371L;

	private ListToVectorFunction() {
		super("Creates a fresh simple general vector from the provided list.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LIST_TO_VECTOR.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = new SymbolStruct<>("LIST-ARG", GlobalPackageStruct.SYSTEM);
		final ParameterAllocation listArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(listArgSymbol, listArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

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

		final ListStruct list = (ListStruct) lispStructs[0];
		return listToVector(list);
	}

	public LispStruct listToVector(final ListStruct list) {
		return new VectorStruct<>(list.getAsJavaList());
	}
}
