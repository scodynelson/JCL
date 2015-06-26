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
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LogIorFunction extends FunctionStruct {

	public static final SymbolStruct<?> LOGIOR = GlobalPackageStruct.COMMON_LISP.intern("LOGIOR").getSymbol();

	private static final long serialVersionUID = -6633197856681204953L;

	@Autowired
	private Printer printer;

	private LogIorFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LOGIOR.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LOGIOR);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> restArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGERS").getSymbol();
		final RestBinding restBinding = new RestBinding(restArgSymbol);

		return new OrdinaryLambdaListBindings.Builder().restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final IntegerStruct[] integers = getIntegers(lispStructs);
		return IntegerStruct.logIor(integers);
	}

	private IntegerStruct[] getIntegers(final LispStruct... lispStructs) {

		final IntegerStruct[] numbers = new IntegerStruct[lispStructs.length];
		for (int i = 0; i < lispStructs.length; i++) {
			final LispStruct lispStruct = lispStructs[i];
			if (lispStruct instanceof IntegerStruct) {
				numbers[i] = (IntegerStruct) lispStruct;
			} else {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("Argument not of type Integer: " + printedObject);
			}
		}
		return numbers;
	}
}