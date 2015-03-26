/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
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

public class ListFunction extends FunctionStruct {

	public static final ListFunction INSTANCE = new ListFunction();

	public static final SymbolStruct<?> LIST = new SymbolStruct<>("LIST", GlobalPackageStruct.COMMON_LISP, null, INSTANCE);

	private static final long serialVersionUID = -4167883057835187873L;

	private ListFunction() {
		super("Returns a list containing the supplied objects.", getInitLambdaListBindings());
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> objectRestArgSymbol = new SymbolStruct<>("objects", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation objectRestArgArgAllocation = new ParameterAllocation(0);
		final RestBinding restBinding = new RestBinding(objectRestArgSymbol, objectRestArgArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return ListStruct.buildProperList(lispStructs);
	}
}
