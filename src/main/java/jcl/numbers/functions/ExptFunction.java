/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ExptFunction extends FunctionStruct {

	public static final SymbolStruct<?> EXPT = GlobalPackageStruct.COMMON_LISP.intern("EXPT").getSymbol();

	private static final long serialVersionUID = -3137653647023696113L;

	@Autowired
	private Printer printer;

	private ExptFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		EXPT.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(EXPT);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> baseNumberSymbol = GlobalPackageStruct.COMMON_LISP.intern("BASE-NUMBER").getSymbol();
		final RequiredBinding requiredBinding1 = new RequiredBinding(baseNumberSymbol);
		requiredBindings.add(requiredBinding1);

		final SymbolStruct<?> powerNumberSymbol = GlobalPackageStruct.COMMON_LISP.intern("POWER-NUMBER").getSymbol();
		final RequiredBinding requiredBinding2 = new RequiredBinding(powerNumberSymbol);
		requiredBindings.add(requiredBinding2);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		if (!(lispStruct1 instanceof NumberStruct)) {
			final String printedObject = printer.print(lispStruct1);
			throw new TypeErrorException("Argument not of type Number: " + printedObject);
		}
		final NumberStruct baseNumber = (NumberStruct) lispStruct1;

		final LispStruct lispStruct2 = lispStructs[1];
		if (!(lispStruct2 instanceof NumberStruct)) {
			final String printedObject = printer.print(lispStruct2);
			throw new TypeErrorException("Argument not of type Number: " + printedObject);
		}
		final NumberStruct powerNumber = (NumberStruct) lispStruct2;

		return baseNumber.expt(powerNumber);
	}
}
