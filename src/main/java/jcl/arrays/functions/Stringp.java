/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.arrays.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class Stringp extends FunctionStruct {

	public static final SymbolStruct STRINGP = GlobalPackageStruct.COMMON_LISP.intern("STRINGP").getSymbol();

	private static final long serialVersionUID = 8175736295020631918L;

	private Stringp() {
		super("Returns T if object is a STRING; otherwise, returns NIL.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		STRINGP.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(STRINGP);
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

		return (lispStructs[0] instanceof StringStruct) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
