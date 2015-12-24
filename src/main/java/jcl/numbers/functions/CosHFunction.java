/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CosHFunction extends FunctionStruct {

	public static final SymbolStruct COSH = GlobalPackageStruct.COMMON_LISP.intern("COSH").getSymbol();

	private static final long serialVersionUID = -4866561095341902549L;

	@Autowired
	private Printer printer;

	private CosHFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		COSH.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(COSH);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBER").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof NumberStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Number: " + printedObject);
		}

		final NumberStruct number = (NumberStruct) lispStruct;
		return number.cosh();
	}
}
