/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class MaxFunction extends FunctionStruct {

	public static final SymbolStruct<?> MAX = GlobalPackageStruct.COMMON_LISP.intern("MAX").getSymbol();

	private static final long serialVersionUID = 8175787423495063531L;

	@Autowired
	private Printer printer;

	private MaxFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MAX.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MAX);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> restArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REALS").getSymbol();
		final RestBinding restBinding = new RestBinding(restArgSymbol);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .restBinding(restBinding)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final RealStruct[] reals = getReals(lispStructs);
		return RealStruct.max(reals);
	}

	private RealStruct[] getReals(final LispStruct... lispStructs) {

		final RealStruct[] numbers = new RealStruct[lispStructs.length];
		for (int i = 0; i < lispStructs.length; i++) {
			final LispStruct lispStruct = lispStructs[i];
			if (lispStruct instanceof RealStruct) {
				numbers[i] = (RealStruct) lispStruct;
			} else {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("Argument not of type Real: " + printedObject);
			}
		}
		return numbers;
	}
}