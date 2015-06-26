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
import jcl.numbers.NumberStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ATanFunction extends FunctionStruct {

	public static final SymbolStruct<?> ATAN = GlobalPackageStruct.COMMON_LISP.intern("ATAN").getSymbol();

	private static final long serialVersionUID = -543069078158302923L;

	@Autowired
	private Printer printer;

	private ATanFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		ATAN.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(ATAN);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBER1").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> optionalArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBER2").getSymbol();

		final SymbolStruct<?> optionalSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("NUMBER2-P-" + System.nanoTime()).getSymbol();
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

		if (lispStructs.length == 1) {
			final LispStruct lispStruct = lispStructs[0];
			if (!(lispStruct instanceof NumberStruct)) {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("Argument not of type Number: " + printedObject);
			}

			final NumberStruct number = (NumberStruct) lispStruct;
			return number.atan();
		} else {
			final LispStruct lispStruct1 = lispStructs[0];
			if (!(lispStruct1 instanceof RealStruct)) {
				final String printedObject = printer.print(lispStruct1);
				throw new TypeErrorException("Argument not of type Real: " + printedObject);
			}
			final RealStruct real1 = (RealStruct) lispStruct1;

			final LispStruct lispStruct2 = lispStructs[1];
			if (!(lispStruct2 instanceof RealStruct)) {
				final String printedObject = printer.print(lispStruct2);
				throw new TypeErrorException("Argument not of type Real: " + printedObject);
			}
			final RealStruct real2 = (RealStruct) lispStruct2;

			return real1.atan(real2);
		}
	}
}