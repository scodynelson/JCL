/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler;

import java.util.Arrays;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class ValuesFunction extends FunctionStruct {

	public static final SymbolStruct VALUES = GlobalPackageStruct.COMMON_LISP.intern("VALUES").getSymbol();

	private static final long serialVersionUID = -7869325469764526281L;

	private ValuesFunction() {
		super("Returns the objects as multiple values.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		VALUES.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(VALUES);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct objectRestArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECTS").getSymbol();
		final RestParameter restBinding = new RestParameter(objectRestArgSymbol);

		return new OrdinaryLambdaList.Builder().restBinding(restBinding)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return values(lispStructs);
	}

	public ValuesStruct values(final LispStruct... lispStructs) {
		final List<LispStruct> valuesList = Arrays.asList(lispStructs);
		return new ValuesStruct(valuesList);
	}
}
