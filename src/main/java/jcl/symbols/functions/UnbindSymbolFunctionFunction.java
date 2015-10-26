/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
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

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> symbolArgSymbol = GlobalPackageStruct.SYSTEM.intern("SYM").getSymbol();
		final RequiredParameter symbolArgRequiredBinding = new RequiredParameter(symbolArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(symbolArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		return symbol.unbindFunction();
	}
}
