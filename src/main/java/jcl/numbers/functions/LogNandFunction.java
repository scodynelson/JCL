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
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LogNandFunction extends FunctionStruct {

	public static final SymbolStruct<?> LOGNAND = GlobalPackageStruct.COMMON_LISP.intern("LOGNAND").getSymbol();

	private static final long serialVersionUID = 2944108957771218524L;

	@Autowired
	private Printer printer;

	private LogNandFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LOGNAND.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LOGNAND);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> integer1Symbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER-1").getSymbol();
		final RequiredParameter requiredBinding1 = new RequiredParameter(integer1Symbol);
		requiredBindings.add(requiredBinding1);

		final SymbolStruct<?> integer2Symbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER-2").getSymbol();
		final RequiredParameter requiredBinding2 = new RequiredParameter(integer2Symbol);
		requiredBindings.add(requiredBinding2);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		if (!(lispStruct1 instanceof IntegerStruct)) {
			final String printedObject = printer.print(lispStruct1);
			throw new TypeErrorException("Argument not of type Integer: " + printedObject);
		}
		final IntegerStruct integer1 = (IntegerStruct) lispStruct1;

		final LispStruct lispStruct2 = lispStructs[1];
		if (!(lispStruct2 instanceof IntegerStruct)) {
			final String printedObject = printer.print(lispStruct2);
			throw new TypeErrorException("Argument not of type Integer: " + printedObject);
		}
		final IntegerStruct integer2 = (IntegerStruct) lispStruct2;

		return integer1.logNand(integer2);
	}
}
