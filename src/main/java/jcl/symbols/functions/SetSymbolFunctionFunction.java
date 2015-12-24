/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolFunctionFunction extends AbstractSystemFunctionStruct {

	private static final long serialVersionUID = 1025657474175401906L;

	public SetSymbolFunctionFunction() {
		super("Sets the function value of the provided symbol to the provided function value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final RequiredParameter symbolArg = new RequiredParameter.Builder(GlobalPackageStruct.SYSTEM, "SYMBOL").build();
		final RequiredParameter functionArg = new RequiredParameter.Builder(GlobalPackageStruct.SYSTEM, "FUNCTION").build();
		return Arrays.asList(symbolArg, functionArg);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol = (SymbolStruct) lispStructs[0];
		final FunctionStruct function = (FunctionStruct) lispStructs[1];
		symbol.setFunction(function);
		return function;
	}

	@Override
	protected String functionName() {
		return "SET-SYMBOL-FUNCTION";
	}
}
