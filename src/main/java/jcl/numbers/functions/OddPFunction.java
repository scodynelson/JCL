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
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class OddPFunction extends FunctionStruct {

	public static final SymbolStruct<?> ODDP = GlobalPackageStruct.COMMON_LISP.intern("ODDP").getSymbol();

	private static final long serialVersionUID = -4319243135134408847L;

	@Autowired
	private Printer printer;

	private OddPFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		ODDP.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(ODDP);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof IntegerStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Integer: " + printedObject);
		}

		final IntegerStruct integer = (IntegerStruct) lispStruct;
		return integer.oddp() ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
