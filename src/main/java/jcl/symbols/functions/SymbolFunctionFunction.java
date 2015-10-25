/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SymbolFunctionFunction extends FunctionStruct {

	public static final SymbolStruct<?> SYMBOL_FUNCTION = GlobalPackageStruct.COMMON_LISP.intern("SYMBOL-FUNCTION").getSymbol();

	private static final long serialVersionUID = 1025657474175401906L;

	private SymbolFunctionFunction() {
		super("Gets the function value of the provided symbol.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		SYMBOL_FUNCTION.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(SYMBOL_FUNCTION);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> symbolArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("SYM").getSymbol();
		final RequiredParameter symbolArgRequiredBinding = new RequiredParameter(symbolArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(symbolArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
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
