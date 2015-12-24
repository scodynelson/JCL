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
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.NumberStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class NotEqualToFunction extends FunctionStruct {

	public static final SymbolStruct NOT_EQUAL_TO = GlobalPackageStruct.COMMON_LISP.intern("/=").getSymbol();

	private static final long serialVersionUID = -8966405788493785225L;

	@Autowired
	private Printer printer;

	private NotEqualToFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		NOT_EQUAL_TO.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(NOT_EQUAL_TO);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBER").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct restArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBERS").getSymbol();
		final RestParameter restBinding = new RestParameter(restArgSymbol);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final NumberStruct[] numbers = getNumbers(lispStructs);
		return NumberStruct.isNotEqualTo(numbers) ? TStruct.INSTANCE : NILStruct.INSTANCE;
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
