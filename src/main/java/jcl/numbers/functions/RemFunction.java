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
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class RemFunction extends FunctionStruct {

	public static final SymbolStruct<?> REM = GlobalPackageStruct.COMMON_LISP.intern("REM").getSymbol();

	private static final long serialVersionUID = 4003956118205210262L;

	@Autowired
	private Printer printer;

	private RemFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		REM.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(REM);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> realSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredBinding requiredBinding1 = new RequiredBinding(realSymbol);
		requiredBindings.add(requiredBinding1);

		final SymbolStruct<?> divisorSymbol = GlobalPackageStruct.COMMON_LISP.intern("DIVISOR").getSymbol();
		final RequiredBinding requiredBinding2 = new RequiredBinding(divisorSymbol);
		requiredBindings.add(requiredBinding2);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		if (!(lispStruct1 instanceof RealStruct)) {
			final String printedObject = printer.print(lispStruct1);
			throw new TypeErrorException("Argument not of type Real: " + printedObject);
		}
		final RealStruct real = (RealStruct) lispStruct1;

		final LispStruct lispStruct2 = lispStructs[1];
		if (!(lispStruct2 instanceof RealStruct)) {
			final String printedObject = printer.print(lispStruct2);
			throw new TypeErrorException("Argument not of type Real: " + printedObject);
		}
		final RealStruct divisor = (RealStruct) lispStruct2;

		return real.rem(divisor);
	}
}