/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class CarFunction extends FunctionStruct {

	public static final SymbolStruct CAR = GlobalPackageStruct.COMMON_LISP.intern("CAR").getSymbol();

	private static final long serialVersionUID = -4167883057835187873L;

	private CarFunction() {
		super("Gets the car of the provided list.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		CAR.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(CAR);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("LIST-ARG").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final ListStruct list = (ListStruct) lispStructs[0];
		return car(list);
	}

	public LispStruct car(final ListStruct list) {
		return list.getFirst();
	}
}
