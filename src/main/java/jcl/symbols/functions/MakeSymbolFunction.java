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
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MakeSymbolFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public MakeSymbolFunction() {
		super("Creates and returns a fresh, uninterned symbol whose name is the given name.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "NAME").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final StringStruct name
				= validator.validateType(lispStructs[0], functionName(), "Name", StringType.INSTANCE, StringStruct.class);
		return name.asSymbol().get();
	}

	@Override
	protected String functionName() {
		return "MAKE-SYMBOL";
	}
}
