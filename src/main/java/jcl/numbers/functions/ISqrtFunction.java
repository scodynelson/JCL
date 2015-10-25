/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ISqrtFunction extends FunctionStruct {

	public static final SymbolStruct<?> ISQRT = GlobalPackageStruct.COMMON_LISP.intern("ISQRT").getSymbol();

	private static final long serialVersionUID = 8041228069336501886L;

	@Autowired
	private Printer printer;

	private ISqrtFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		ISQRT.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(ISQRT);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof IntegerStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Integer: " + printedObject);
		}

		final IntegerStruct integer = (IntegerStruct) lispStruct;
		return integer.isqrt();
	}
}
