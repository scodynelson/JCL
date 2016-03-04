/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ListFunction extends AbstractCommonLispFunctionStruct {

	public static final SymbolStruct LIST = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbol();

	public ListFunction() {
		super("Returns a list containing the supplied objects.");
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECTS").build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);
		return ListStruct.buildProperList(lispStructs);
	}

	@Override
	protected String functionName() {
		return "LIST";
	}
}
