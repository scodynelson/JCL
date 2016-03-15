/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
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

	public static final SymbolStruct LOGIOR = GlobalPackageStruct.COMMON_LISP.intern("LOGIOR").getSymbol();

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

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct restArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGERS").getSymbol();
		final RestParameter restBinding = new RestParameter(restArgSymbol);

		return OrdinaryLambdaList.builder()
		                         .restBinding(restBinding)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

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
