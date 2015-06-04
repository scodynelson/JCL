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
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class LogFunction extends FunctionStruct {

	public static final SymbolStruct<?> LOG = GlobalPackageStruct.COMMON_LISP.intern("LOG").getSymbol();

	private static final long serialVersionUID = 389567032159715710L;

	@Autowired
	private Printer printer;

	private LogFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		LOG.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(LOG);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NUMBER").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> optionalArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("BASE").getSymbol();

		final SymbolStruct<?> optionalSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("BASE-P-" + System.nanoTime()).getSymbol();
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
		if (!(lispStruct instanceof NumberStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Number: " + printedObject);
		}
		final NumberStruct number = (NumberStruct) lispStruct;

		if (lispStructs.length > 1) {
			final LispStruct base = lispStructs[1];
			if (!(base instanceof NumberStruct)) {
				final String printedObject = printer.print(base);
				throw new TypeErrorException("Argument not of type Number: " + printedObject);
			}
			final NumberStruct baseNumber = (NumberStruct) base;

			return number.log(baseNumber);
		} else {
			return number.log();
		}
	}
}
