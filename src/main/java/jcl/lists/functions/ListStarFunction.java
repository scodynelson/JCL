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
public final class ListStarFunction extends AbstractCommonLispFunctionStruct {

	public static final SymbolStruct LIST_STAR = GlobalPackageStruct.COMMON_LISP.intern("LIST*").getSymbol();

	public ListStarFunction() {
		super("Returns a list containing the supplied objects where the last argument becomes the cdr of the last cons constructed.");
	}

	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECTS").build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		if (lispStructs.length == 1) {
			return lispStructs[0];
		}
		return ListStruct.buildDottedList(lispStructs);
	}

	@Override
	protected String functionName() {
		return "LIST*";
	}
}
