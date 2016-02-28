/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolValueFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetSymbolValueFunction() {
		super("Sets the value of the provided symbol to the provided value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final RequiredParameter symbolArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "SYMBOL").build();
		final RequiredParameter valueArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "VALUE").build();
		return Arrays.asList(symbolArg, valueArg);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol =
				validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);
		final LispStruct value = lispStructs[1];

		symbol.setValue(value);
		return value;
	}

	@Override
	protected String functionName() {
		return "SET-SYMBOL-VALUE";
	}
}
