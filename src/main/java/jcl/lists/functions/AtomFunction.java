/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;
import org.springframework.stereotype.Component;

@Component
public final class AtomFunction extends AbstractCommonLispFunctionStruct {

	public AtomFunction() {
		super("Returns T if object is of type atom; otherwise, returns NIL.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct object = lispStructs[0];
		return BooleanStructs.toLispBoolean(!(object instanceof ConsStruct));
	}

	@Override
	protected String functionName() {
		return "ATOM";
	}
}
