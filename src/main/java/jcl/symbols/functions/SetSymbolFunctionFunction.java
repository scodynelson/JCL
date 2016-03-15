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
import jcl.types.CompiledFunctionType;
import jcl.types.FunctionType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolFunctionFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetSymbolFunctionFunction() {
		super("Sets the function value of the provided symbol to the provided function value.");
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
		validator.validateTypes(lispStructs[1], functionName(), "Function", FunctionType.INSTANCE, CompiledFunctionType.INSTANCE);
		final FunctionStruct function = (FunctionStruct) lispStructs[1];

		symbol.setFunction(function);
		return function;
	}

	@Override
	protected String functionName() {
		return "SET-SYMBOL-FUNCTION";
	}
}
