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
import jcl.numbers.RationalStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class NumeratorFunction extends FunctionStruct {

	public static final SymbolStruct NUMERATOR = GlobalPackageStruct.COMMON_LISP.intern("NUMERATOR").getSymbol();

	@Autowired
	private Printer printer;

	private NumeratorFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NUMERATOR.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(NUMERATOR);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("RATIONAL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof RationalStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Rational: " + printedObject);
		}

		final RationalStruct rational = (RationalStruct) lispStruct;
		return rational.numerator();
	}
}
