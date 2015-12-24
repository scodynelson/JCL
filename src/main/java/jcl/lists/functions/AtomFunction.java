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
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class AtomFunction extends FunctionStruct {

	public static final SymbolStruct ATOM = GlobalPackageStruct.COMMON_LISP.intern("ATOM").getSymbol();

	private static final long serialVersionUID = 2155686329445366209L;

	private AtomFunction() {
		super("Returns T if object is of type atom; otherwise, returns NIL.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		ATOM.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(ATOM);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return atom(lispStructs[0]);
	}

	public BooleanStruct atom(final LispStruct object) {
		return (object instanceof ConsStruct) ? NILStruct.INSTANCE : TStruct.INSTANCE;
	}
}
