/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.ArrayList;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5517697908893213287L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetFunction() {
		super("Gets the function value of the provided symbol.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final List<RequiredParameter> requiredParameters = new ArrayList<>(2);

		final RequiredParameter symbol = new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL").build();
		requiredParameters.add(symbol);

		final RequiredParameter value = new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "VALUE").build();
		requiredParameters.add(value);

		return requiredParameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		validator.validateTypes(lispStruct1, functionName(), "Symbol", SymbolType.INSTANCE);

		final LispStruct lispStruct2 = lispStructs[1];

		final SymbolStruct symbol = (SymbolStruct) lispStruct1;
		symbol.setValue(lispStruct2);

		return symbol.getValue();
	}

	@Override
	protected String functionName() {
		return "SET";
	}
}
