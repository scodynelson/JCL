/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GetFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public GetFunction() {
		super("Finds a property on the property list of symbol whose property indicator is identical to indicator, and returns its corresponding property value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final RequiredParameter symbol = RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL").build();
		final RequiredParameter indicator = RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INDICATOR").build();
		return Arrays.asList(symbol, indicator);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "DEFAULT")
		                        .suppliedPBinding()
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol =
				validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);
		final LispStruct indicator = lispStructs[1];

		final LispStruct defaultValue;
		if (lispStructs.length > 2) {
			defaultValue = lispStructs[2];
		} else {
			defaultValue = NILStruct.INSTANCE;
		}

		return symbol.getProperty(indicator, defaultValue);
	}

	@Override
	protected String functionName() {
		return "GET";
	}
}
