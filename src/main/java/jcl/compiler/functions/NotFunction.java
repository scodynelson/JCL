/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.stereotype.Component;

@Component
public final class NotFunction extends FunctionStruct {

	public static final SymbolStruct NOT = GlobalPackageStruct.COMMON_LISP.intern("NOT").getSymbol();

	private static final long serialVersionUID = 1065457118829768313L;

	private NotFunction() {
		super("Returns T if x is false; otherwise, returns NIL.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NOT.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(NOT);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return notFn(lispStructs[0]);
	}

	public BooleanStruct notFn(final LispStruct object) {
		return notFnJavaBoolean(object) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	public boolean notFnJavaBoolean(final LispStruct object) {
		return NullStruct.INSTANCE.equals(object) || NILStruct.INSTANCE.equals(object);
	}
}
