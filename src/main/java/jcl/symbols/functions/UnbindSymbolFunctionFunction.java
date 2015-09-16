/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class UnbindSymbolFunctionFunction extends FunctionStruct {

	public static final SymbolStruct<?> UNBIND_SYMBOL_FUNCTION = GlobalPackageStruct.SYSTEM.intern("UNBIND-SYMBOL-FUNCTION").getSymbol();

	private static final long serialVersionUID = -5643412206135382825L;

	private UnbindSymbolFunctionFunction() {
		super("Unbinds the function value of the provided symbol from its current value.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		UNBIND_SYMBOL_FUNCTION.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(UNBIND_SYMBOL_FUNCTION);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> symbolArgSymbol = GlobalPackageStruct.SYSTEM.intern("SYM").getSymbol();
		final RequiredBinding symbolArgRequiredBinding = new RequiredBinding(symbolArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(symbolArgRequiredBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		return symbol.unbindFunction();
	}
}
