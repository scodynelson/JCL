/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ConsFunction extends FunctionStruct {

	public static final SymbolStruct CONS = GlobalPackageStruct.COMMON_LISP.intern("CONS").getSymbol();

	private static final long serialVersionUID = 1242798660975184815L;

	private ConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		CONS.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(CONS);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct object1ArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT-1").getSymbol();
		final RequiredParameter object1RequiredBinding = new RequiredParameter(object1ArgSymbol);
		requiredBindings.add(object1RequiredBinding);

		final SymbolStruct object2ArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT-2").getSymbol();
		final RequiredParameter object2RequiredBinding = new RequiredParameter(object2ArgSymbol);
		requiredBindings.add(object2RequiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return cons(lispStructs[0], lispStructs[1]);
	}

	public LispStruct cons(final LispStruct object1, final LispStruct object2) {
		return new ConsStruct(object1, object2);
	}
}
