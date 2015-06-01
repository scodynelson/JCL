/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.FloatStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FloatSignFunction extends FunctionStruct {

	public static final SymbolStruct<?> FLOAT_SIGN = GlobalPackageStruct.COMMON_LISP.intern("FLOAT-SIGN").getSymbol();

	private static final long serialVersionUID = 6851077465479293675L;

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

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FLOAT1").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> optionalArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("FLOAT2").getSymbol();

		final SymbolStruct<?> optionalSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("FLOAT2-P-" + System.nanoTime()).getSymbol();
		final SuppliedPBinding optionalSuppliedPBinding = new SuppliedPBinding(optionalSuppliedP);

		final OptionalBinding optionalBinding = new OptionalBinding(optionalArgSymbol, NullStruct.INSTANCE, optionalSuppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(optionalBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .optionalBindings(optionalBindings)
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
		final FloatStruct float1 = (FloatStruct) lispStruct;

		if (lispStructs.length > 1) {
			final LispStruct imaginary = lispStructs[1];
			if (!(imaginary instanceof FloatStruct)) {
				final String printedObject = printer.print(imaginary);
				throw new TypeErrorException("Argument not of type Float: " + printedObject);
			}
			final FloatStruct float2 = (FloatStruct) lispStruct;

			return float1.floatSign(float2);
		} else {
			return float1.floatSign();
		}
	}
}
