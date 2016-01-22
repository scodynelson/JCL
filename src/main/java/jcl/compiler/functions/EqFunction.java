/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class EqFunction extends FunctionStruct {

	public static final SymbolStruct EQ = GlobalPackageStruct.COMMON_LISP.intern("EQ").getSymbol();

	private static final long serialVersionUID = 4849096028160792790L;

	private EqFunction() {
		super("Returns true if its arguments are the same, identical object; otherwise, returns false.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		EQ.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(EQ);
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

		return eq(lispStructs[0], lispStructs[1]);
	}

	public BooleanStruct eq(final LispStruct object1, final LispStruct object2) {
		// TODO: This is right, but not all of our objects are right for this. Numbers for example and maybe symbols as well.
		return (object1 == object2) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
