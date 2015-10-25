/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ModFunction extends FunctionStruct {

	public static final SymbolStruct<?> MOD = GlobalPackageStruct.COMMON_LISP.intern("MOD").getSymbol();

	private static final long serialVersionUID = 5684887328457930680L;

	@Autowired
	private Printer printer;

	private ModFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		MOD.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(MOD);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> realSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredParameter requiredBinding1 = new RequiredParameter(realSymbol);
		requiredBindings.add(requiredBinding1);

		final SymbolStruct<?> divisorSymbol = GlobalPackageStruct.COMMON_LISP.intern("DIVISOR").getSymbol();
		final RequiredParameter requiredBinding2 = new RequiredParameter(divisorSymbol);
		requiredBindings.add(requiredBinding2);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
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

		return real.mod(divisor);
	}
}
