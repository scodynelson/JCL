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
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class NthFunction extends FunctionStruct {

	public static final SymbolStruct NTH = GlobalPackageStruct.COMMON_LISP.intern("NTH").getSymbol();

	private static final long serialVersionUID = 2998151869440671653L;

	private NthFunction() {
		super("Locates the nth element of list, where the car of the list is the ``zeroth'' element.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NTH.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(NTH);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct object1ArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("N").getSymbol();
		final RequiredParameter object1RequiredBinding = new RequiredParameter(object1ArgSymbol);
		requiredBindings.add(object1RequiredBinding);

		final SymbolStruct object2ArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("LIST").getSymbol();
		final RequiredParameter object2RequiredBinding = new RequiredParameter(object2ArgSymbol);
		requiredBindings.add(object2RequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final IntegerStruct index = (IntegerStruct) lispStructs[0];
		final ListStruct list = (ListStruct) lispStructs[1];

		return list.getElement(index.getBigInteger().intValue());
	}
}
