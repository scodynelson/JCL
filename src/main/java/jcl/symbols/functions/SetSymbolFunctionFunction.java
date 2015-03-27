/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.ArrayList;
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
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;

public class SetSymbolFunctionFunction extends FunctionStruct {

	public static final SetSymbolFunctionFunction INSTANCE = new SetSymbolFunctionFunction();

	public static final SymbolStruct<?> SET_SYMBOL_FUNCTION = new SymbolStruct<>("SET-SYMBOL-FUNCTION", GlobalPackageStruct.COMMON_LISP, null, INSTANCE);

	private static final long serialVersionUID = 1025657474175401906L;

	private SetSymbolFunctionFunction() {
		super("Sets the function value of the provided symbol to the provided function value.", getInitLambdaListBindings());
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> symbolArgSymbol = new SymbolStruct<>("symbol", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation symbolArgAllocation = new ParameterAllocation(0);
		final RequiredBinding symbolArgRequiredBinding = new RequiredBinding(symbolArgSymbol, symbolArgAllocation);
		requiredBindings.add(symbolArgRequiredBinding);

		final SymbolStruct<?> functionArgSymbol = new SymbolStruct<>("function", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation functionArgAllocation = new ParameterAllocation(0);
		final RequiredBinding functionArgRequiredBinding = new RequiredBinding(functionArgSymbol, functionArgAllocation);
		requiredBindings.add(functionArgRequiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		final FunctionStruct function = (FunctionStruct) lispStructs[1];
		symbol.setFunction(function);
		return function;
	}
}