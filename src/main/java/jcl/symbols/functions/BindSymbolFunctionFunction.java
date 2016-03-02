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
import jcl.types.FunctionType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class BindSymbolFunctionFunction extends AbstractSystemFunctionStruct {

	// TODO: Get rid of this??
	public static final SymbolStruct BIND_SYMBOL_FUNCTION = GlobalPackageStruct.SYSTEM.intern("BIND-SYMBOL-FUNCTION").getSymbol();

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public BindSymbolFunctionFunction() {
		super("Binds the function value of the provided symbol to the provided function value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final RequiredParameter symbolArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "SYMBOL").build();
		final RequiredParameter functionArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "FUNCTION").build();
		return Arrays.asList(symbolArg, functionArg);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol =
				validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);
		final FunctionStruct function =
				validator.validateType(lispStructs[1], functionName(), "Function", FunctionType.INSTANCE, FunctionStruct.class);

		symbol.bindFunction(function);
		return new ValuesStruct();
	}

	@Override
	protected String functionName() {
		return "BIND-SYMBOL-FUNCTION";
	}
}
