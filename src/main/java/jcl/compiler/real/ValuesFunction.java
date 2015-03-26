/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real;

import java.util.Arrays;
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
import jcl.compiler.real.struct.ValuesStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

public class ValuesFunction extends FunctionStruct {

	public static final ValuesFunction INSTANCE = new ValuesFunction();

	public static final SymbolStruct<?> VALUES = new SymbolStruct<>("VALUES", GlobalPackageStruct.COMMON_LISP, null, INSTANCE);

	private static final long serialVersionUID = -7869325469764526281L;

	private ValuesFunction() {
		super("Returns the objects as multiple values.", getInitLambdaListBindings());
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = Collections.emptyList();
		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final SymbolStruct<?> objectRestArgSymbol = new SymbolStruct<>("object", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation objectRestArgArgAllocation = new ParameterAllocation(0);
		final RestBinding restBinding = new RestBinding(objectRestArgSymbol, objectRestArgArgAllocation);

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final List<LispStruct> valuesList = Arrays.asList(lispStructs);
		return new ValuesStruct(valuesList);
	}
}
