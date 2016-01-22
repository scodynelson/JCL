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
public final class DenominatorFunction extends FunctionStruct {

	public static final SymbolStruct DENOMINATOR = GlobalPackageStruct.COMMON_LISP.intern("DENOMINATOR").getSymbol();

	private static final long serialVersionUID = 6210778370087072605L;

	@Autowired
	private Printer printer;

	private DenominatorFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		DENOMINATOR.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(DENOMINATOR);
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
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof RationalStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Rational: " + printedObject);
		}

		final RationalStruct rational = (RationalStruct) lispStruct;
		return rational.denominator();
	}
}
