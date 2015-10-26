/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.ArrayList;
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
public class SetSymbolFunctionFunction extends FunctionStruct {

	public static final SymbolStruct<?> SET_SYMBOL_FUNCTION = GlobalPackageStruct.SYSTEM.intern("SET-SYMBOL-FUNCTION").getSymbol();

	private static final long serialVersionUID = 1025657474175401906L;

	private SetSymbolFunctionFunction() {
		super("Sets the function value of the provided symbol to the provided function value.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		SET_SYMBOL_FUNCTION.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(SET_SYMBOL_FUNCTION);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> symbolArgSymbol = GlobalPackageStruct.SYSTEM.intern("SYM").getSymbol();
		final RequiredParameter symbolArgRequiredBinding = new RequiredParameter(symbolArgSymbol);
		requiredBindings.add(symbolArgRequiredBinding);

		final SymbolStruct<?> functionArgSymbol = GlobalPackageStruct.SYSTEM.intern("FN").getSymbol();
		final RequiredParameter functionArgRequiredBinding = new RequiredParameter(functionArgSymbol);
		requiredBindings.add(functionArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		final FunctionStruct function = (FunctionStruct) lispStructs[1];
		return setSymbolFunction(symbol, function);
	}

	public LispStruct setSymbolFunction(final SymbolStruct<?> symbol, final FunctionStruct function) {
		symbol.setFunction(function);
		return function;
	}
}
