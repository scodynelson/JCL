/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

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
import org.springframework.stereotype.Component;

@Component
public class SymbolFunctionFunction extends FunctionStruct {

	public static final SymbolStruct<?> SYMBOL_FUNCTION = new SymbolStruct<>("SYMBOL-FUNCTION", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 1025657474175401906L;

	private SymbolFunctionFunction() {
		super("Gets the function value of the provided symbol.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		SYMBOL_FUNCTION.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> symbolArgSymbol = new SymbolStruct<>("symbol", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation symbolArgAllocation = new ParameterAllocation(0);
		final RequiredBinding symbolArgRequiredBinding = new RequiredBinding(symbolArgSymbol, symbolArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(symbolArgRequiredBinding);

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

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		return symbolFunction(symbol);
	}

	public LispStruct symbolFunction(final SymbolStruct<?> symbol) {
		return symbol.getFunction();
	}
}
