/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ListStarFunction extends FunctionStruct {

	public static final SymbolStruct LIST_STAR = GlobalPackageStruct.COMMON_LISP.intern("LIST*").getSymbol();

	private static final long serialVersionUID = -5968990222195634426L;

	private ListStarFunction() {
		super("Returns a list containing the supplied objects where the last argument becomes the cdr of the last cons constructed.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LIST_STAR.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LIST_STAR);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct objectRestArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECTS").getSymbol();
		final RestParameter restBinding = new RestParameter(objectRestArgSymbol);

		return OrdinaryLambdaList.builder()
		                         .restBinding(restBinding)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return listStar(lispStructs);
	}

	public LispStruct listStar(final LispStruct... lispStructs) {
		if (lispStructs.length == 1) {
			return lispStructs[0];
		}
		return ListStruct.buildDottedList(lispStructs);
	}
}
