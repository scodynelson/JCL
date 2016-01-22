/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ListToVectorFunction extends FunctionStruct {

	public static final SymbolStruct LIST_TO_VECTOR = GlobalPackageStruct.SYSTEM.intern("LIST-TO-VECTOR").getSymbol();

	private static final long serialVersionUID = 7202751615463953371L;

	private ListToVectorFunction() {
		super("Creates a fresh simple general vector from the provided list.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LIST_TO_VECTOR.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(LIST_TO_VECTOR);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listArgSymbol = GlobalPackageStruct.SYSTEM.intern("LIST-ARG").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final ListStruct list = (ListStruct) lispStructs[0];
		return listToVector(list);
	}

	public LispStruct listToVector(final ListStruct list) {
		return new VectorStruct<>(list.getAsJavaList());
	}
}
