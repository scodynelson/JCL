/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.math.BigDecimal;
import java.math.BigInteger;
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
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberVariables;
import jcl.numbers.RandomStateStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class RandomFunction extends FunctionStruct {

	public static final SymbolStruct RANDOM = GlobalPackageStruct.COMMON_LISP.intern("RANDOM").getSymbol();

	@Autowired
	private Printer printer;

	private RandomFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		RANDOM.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(RANDOM);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("LIMIT").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct optionalArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("RANDOM-STATE").getSymbol();

		final SymbolStruct optionalSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("RANDOM-STATE-P-" + System.nanoTime()).getSymbol();
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

		final RandomStateStruct randomState;
		if (lispStructs.length == 2) {
			final LispStruct lispStruct2 = lispStructs[1];
			if (!(lispStruct2 instanceof RandomStateStruct)) {
				final String printedObject = printer.print(lispStruct2);
				throw new TypeErrorException("Argument not of type RandomState: " + printedObject);
			}
			randomState = (RandomStateStruct) lispStruct2;
		} else {
			randomState = NumberVariables.RANDOM_STATE.getVariableValue();
		}

		final LispStruct lispStruct = lispStructs[0];
		if (lispStruct instanceof IntegerStruct) {
			final IntegerStruct number = (IntegerStruct) lispStruct;
			final BigInteger bigInteger = number.getBigInteger();
			final BigInteger randomInteger = randomState.randomInteger(bigInteger);
			return new IntegerStruct(randomInteger);
		} else if (lispStruct instanceof FloatStruct) {
			final FloatStruct number = (FloatStruct) lispStruct;
			final BigDecimal bigDecimal = number.getBigDecimal();
			final BigDecimal randomFloat = randomState.randomFloat(bigDecimal);
			return new FloatStruct(randomFloat);
		} else {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Integer or Float: " + printedObject);
		}
	}
}
