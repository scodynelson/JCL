/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.ArrayList;
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
public class GetFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1520639566070967751L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public GetFunction() {
		super("Gets the function value of the provided symbol.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);

		final RequiredParameter symbol = RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL").build();
		requiredParameters.add(symbol);

		final RequiredParameter indicator = RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INDICATOR").build();
		requiredParameters.add(indicator);

		return requiredParameters;
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

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Symbol", SymbolType.INSTANCE);

		final LispStruct lispStruct2 = lispStructs[1];

		final SymbolStruct symbol = (SymbolStruct) lispStruct1;
		final LispStruct property = symbol.getProperty(lispStruct2);

		if (property == null) {
			if (lispStructs.length >= 2) {
				return lispStructs[2];
			} else {
				return NILStruct.INSTANCE;
			}
		} else {
			return property;
		}
	}

	@Override
	protected String functionName() {
		return "GET";
	}
}
