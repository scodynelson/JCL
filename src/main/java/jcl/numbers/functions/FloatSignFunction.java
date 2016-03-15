/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.FloatStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FloatSignFunction extends FunctionStruct {

	public static final SymbolStruct FLOAT_SIGN = GlobalPackageStruct.COMMON_LISP.intern("FLOAT-SIGN").getSymbol();

	@Autowired
	private Printer printer;

	private FloatSignFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		FLOAT_SIGN.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(FLOAT_SIGN);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FLOAT1").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct optionalArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FLOAT2").getSymbol();

		final SymbolStruct optionalSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("FLOAT2-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter optionalSuppliedPBinding = new SuppliedPParameter(optionalSuppliedP);

		final OptionalParameter optionalBinding = new OptionalParameter(optionalArgSymbol, NILStruct.INSTANCE, optionalSuppliedPBinding);
		final List<OptionalParameter> optionalBindings = Collections.singletonList(optionalBinding);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .optionalBindings(optionalBindings)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof FloatStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Float: " + printedObject);
		}
		final FloatStruct float1 = (FloatStruct) lispStruct;

		if (lispStructs.length > 1) {
			final LispStruct lispStruct2 = lispStructs[1];
			if (!(lispStruct2 instanceof FloatStruct)) {
				final String printedObject = printer.print(lispStruct2);
				throw new TypeErrorException("Argument not of type Float: " + printedObject);
			}
			final FloatStruct float2 = (FloatStruct) lispStruct2;

			return float1.floatSign(float2);
		} else {
			return float1.floatSign();
		}
	}
}
