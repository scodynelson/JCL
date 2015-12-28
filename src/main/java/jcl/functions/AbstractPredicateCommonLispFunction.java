/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;

public abstract class AbstractPredicateCommonLispFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = 2069209846321958268L;

	protected AbstractPredicateCommonLispFunction(final String documentation) {
		super(documentation);
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		final LispType type = lispStruct.getType();

		final Boolean result = testType().equals(type);
		return BooleanStructs.toLispBoolean(result);
	}

	protected abstract LispType testType();
}
