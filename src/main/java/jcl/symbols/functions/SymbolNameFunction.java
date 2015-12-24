/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolNameFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 6425434199403262018L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SymbolNameFunction() {
		super("Gets the function value of the provided symbol.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Symbol", SymbolType.INSTANCE);

		final SymbolStruct symbol = (SymbolStruct) lispStructs[0];
		return new StringStruct(symbol.getName());
	}

	@Override
	protected String functionName() {
		return "SYMBOL-NAME";
	}
}
