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
import jcl.numbers.FloatStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FloatPrecisionFunction extends FunctionStruct {

	public static final SymbolStruct<?> FLOAT_PRECISION = GlobalPackageStruct.COMMON_LISP.intern("FLOAT-PRECISION").getSymbol();

	@Autowired
	private Printer printer;

	private FloatPrecisionFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		FLOAT_PRECISION.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(FLOAT_PRECISION);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FLOAT").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof FloatStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Float: " + printedObject);
		}

		final FloatStruct floatVal = (FloatStruct) lispStruct;
		return floatVal.floatPrecision();
	}
}
