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
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LogOrC1Function extends FunctionStruct {

	public static final SymbolStruct<?> LOGORC1 = GlobalPackageStruct.COMMON_LISP.intern("LOGORC1").getSymbol();

	private static final long serialVersionUID = -465216875362677606L;

	@Autowired
	private Printer printer;

	private LogOrC1Function() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LOGORC1.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LOGORC1);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {
		final List<RequiredBinding> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> integer1Symbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER-1").getSymbol();
		final RequiredBinding requiredBinding1 = new RequiredBinding(integer1Symbol);
		requiredBindings.add(requiredBinding1);

		final SymbolStruct<?> integer2Symbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER-2").getSymbol();
		final RequiredBinding requiredBinding2 = new RequiredBinding(integer2Symbol);
		requiredBindings.add(requiredBinding2);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
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

		return integer1.logOrC1(integer2);
	}
}