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
public final class ListFunction extends FunctionStruct {

	public static final SymbolStruct LIST = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbol();

	private static final long serialVersionUID = -4167883057835187873L;

	private ListFunction() {
		super("Returns a list containing the supplied objects.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LIST.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LIST);
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

		return list(lispStructs);
	}

	public LispStruct list(final LispStruct... lispStructs) {
		return ListStruct.buildProperList(lispStructs);
	}
}
