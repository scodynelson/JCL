/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ListFunction extends FunctionStruct {

	public static final SymbolStruct<?> LIST = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbol();

	private static final long serialVersionUID = -4167883057835187873L;

	private ListFunction() {
		super("Returns a list containing the supplied objects.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LIST.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LIST);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> objectRestArgSymbol = new SymbolStruct<>("OBJECTS", GlobalPackageStruct.COMMON_LISP);
		final RestBinding restBinding = new RestBinding(objectRestArgSymbol);

		return new OrdinaryLambdaListBindings.Builder().restBinding(restBinding)
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
