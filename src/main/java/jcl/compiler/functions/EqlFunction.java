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
import jcl.functions.EquatorFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class EqlFunction extends EquatorFunctionStruct {

	public static final SymbolStruct EQL = GlobalPackageStruct.COMMON_LISP.intern("EQL").getSymbol();

	private EqlFunction() {
		// TODO: fix
		super("Returns true if its arguments are the same, identical object; otherwise, returns false.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		EQL.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(EQL);
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

		return lispStructs[0].equals(lispStructs[1]) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
