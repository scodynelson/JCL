/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.FunctionType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolPlistFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetSymbolPlistFunction() {
		super("Sets the plist value of the provided symbol to the provided plist value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final RequiredParameter symbolArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "SYMBOL").build();
		final RequiredParameter plistArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "PLIST").build();
		return Arrays.asList(symbolArg, plistArg);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol =
				validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);
		final ListStruct plist =
				validator.validateType(lispStructs[1], functionName(), "Plist", FunctionType.INSTANCE, ListStruct.class);

		symbol.setProperties(plist);
		return plist;
	}

	@Override
	protected String functionName() {
		return "SET-SYMBOL-PLIST";
	}
}
