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
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.QuotientRemainderResult;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CeilingFunction extends FunctionStruct {

	public static final SymbolStruct<?> CEILING = GlobalPackageStruct.COMMON_LISP.intern("CEILING").getSymbol();

	private static final long serialVersionUID = 4662950665621256298L;

	@Autowired
	private Printer printer;

	private CeilingFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		CEILING.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(CEILING);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> divisorArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("DIVISOR").getSymbol();

		final SymbolStruct<?> divisorSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("DIVISOR-P-" + System.nanoTime()).getSymbol();
		final SuppliedPBinding divisorSuppliedPBinding = new SuppliedPBinding(divisorSuppliedP);

		final OptionalBinding optionalBinding = new OptionalBinding(divisorArgSymbol, IntegerStruct.ONE, divisorSuppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(optionalBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .optionalBindings(optionalBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct = lispStructs[0];
		if (!(lispStruct instanceof RealStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Real: " + printedObject);
		}
		final RealStruct real = (RealStruct) lispStruct;

		final QuotientRemainderResult ceiling;
		if (lispStructs.length > 1) {
			final LispStruct divisor = lispStructs[1];
			if (!(divisor instanceof RealStruct)) {
				final String printedObject = printer.print(divisor);
				throw new TypeErrorException("Argument not of type Real: " + printedObject);
			}
			final RealStruct divisorReal = (RealStruct) divisor;

			ceiling = real.ceiling(divisorReal);
		} else {
			ceiling = real.ceiling();
		}
		final RealStruct quotient = ceiling.getQuotient();
		final RealStruct remainder = ceiling.getRemainder();
		return new ValuesStruct(quotient, remainder);
	}
}