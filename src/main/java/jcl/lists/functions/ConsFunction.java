/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ConsStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ConsFunction extends AbstractCommonLispFunctionStruct {

	public static final SymbolStruct CONS = GlobalPackageStruct.COMMON_LISP.intern("CONS").getSymbol();

	public ConsFunction() {
		super("Creates a fresh cons, the car of which is object-1 and the cdr of which is object-2.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT-1").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT-2").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct object1 = lispStructs[0];
		final LispStruct object2 = lispStructs[1];
		return new ConsStruct(object1, object2);
	}

	@Override
	protected String functionName() {
		return "CONS";
	}
}
