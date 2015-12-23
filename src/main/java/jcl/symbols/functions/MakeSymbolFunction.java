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
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MakeSymbolFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 763642451756794794L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public MakeSymbolFunction() {
		super("Gets the function value of the provided symbol.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		validator.validateTypes(lispStruct, functionName(), "Name", StringType.INSTANCE);

		final StringStruct name = (StringStruct) lispStructs[0];
		return new SymbolStruct<>(name.getAsJavaString());
	}

	@Override
	protected String functionName() {
		return "MAKE-SYMBOL";
	}
}
