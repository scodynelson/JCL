/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPParameter;
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
public final class FRoundFunction extends FunctionStruct {

	public static final SymbolStruct<?> FROUND = GlobalPackageStruct.COMMON_LISP.intern("FROUND").getSymbol();

	private static final long serialVersionUID = 7154427568385187262L;

	@Autowired
	private Printer printer;

	private FRoundFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		FROUND.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(FROUND);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> divisorArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("DIVISOR").getSymbol();

		final SymbolStruct<?> divisorSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("DIVISOR-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter divisorSuppliedPBinding = new SuppliedPParameter(divisorSuppliedP);

		final OptionalParameter optionalBinding = new OptionalParameter(divisorArgSymbol, IntegerStruct.ONE, divisorSuppliedPBinding);
		final List<OptionalParameter> optionalBindings = Collections.singletonList(optionalBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
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

		final QuotientRemainderResult fround;
		if (lispStructs.length > 1) {
			final LispStruct divisor = lispStructs[1];
			if (!(divisor instanceof RealStruct)) {
				final String printedObject = printer.print(divisor);
				throw new TypeErrorException("Argument not of type Real: " + printedObject);
			}
			final RealStruct divisorReal = (RealStruct) divisor;

			fround = real.fround(divisorReal);
		} else {
			fround = real.fround();
		}
		final RealStruct quotient = fround.getQuotient();
		final RealStruct remainder = fround.getRemainder();
		return new ValuesStruct(quotient, remainder);
	}
}
