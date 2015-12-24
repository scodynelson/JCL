/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class BindSymbolFunctionFunction extends AbstractSystemFunctionStruct {

	public static final SymbolStruct BIND_SYMBOL_FUNCTION = GlobalPackageStruct.SYSTEM.intern("BIND-SYMBOL-FUNCTION").getSymbol();

	private static final long serialVersionUID = 1025657474175401906L;

	public BindSymbolFunctionFunction() {
		super("Binds the function value of the provided symbol to the provided function value.");
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
		symbol.bindFunction(function);
		return new ValuesStruct();
	}

	@Override
	protected String functionName() {
		return "BIND-SYMBOL-FUNCTION";
	}
}
