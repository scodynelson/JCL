/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MultiplyFunction extends FunctionStruct {

	public static final SymbolStruct<?> MULTIPLY = GlobalPackageStruct.COMMON_LISP.intern("*").getSymbol();

	private static final long serialVersionUID = -3125334565141339662L;

	@Autowired
	private Printer printer;

	private MultiplyFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MULTIPLY.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MULTIPLY);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> restArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBERS").getSymbol();
		final RestBinding restBinding = new RestBinding(restArgSymbol);

		return new OrdinaryLambdaListBindings.Builder().restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final NumberStruct[] numbers = getNumbers(lispStructs);
		return NumberStruct.multiply(numbers);
	}

	private NumberStruct[] getNumbers(final LispStruct... lispStructs) {

		final NumberStruct[] numbers = new NumberStruct[lispStructs.length];
		for (int i = 0; i < lispStructs.length; i++) {
			final LispStruct lispStruct = lispStructs[i];
			if (lispStruct instanceof NumberStruct) {
				numbers[i] = (NumberStruct) lispStruct;
			} else {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("Argument not of type Number: " + printedObject);
			}
		}
		return numbers;
	}
}
