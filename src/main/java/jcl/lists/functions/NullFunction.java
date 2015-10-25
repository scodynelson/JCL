/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lists.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class NullFunction extends FunctionStruct {

	public static final SymbolStruct<?> NULL = GlobalPackageStruct.COMMON_LISP.intern("NULL").getSymbol();

	private static final long serialVersionUID = 7471817337078296980L;

	private NullFunction() {
		super("Returns T if object is the empty list; otherwise, returns NIL.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NULL.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(NULL);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return nullFn(lispStructs[0]);
	}

	public BooleanStruct nullFn(final LispStruct object) {
		return nullFnJavaBoolean(object) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	public boolean nullFnJavaBoolean(final LispStruct object) {
		return NullStruct.INSTANCE.equals(object) || NILStruct.INSTANCE.equals(object);
	}
}
