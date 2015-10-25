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
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.NumberStruct;
import jcl.numbers.RatioStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.springframework.stereotype.Component;

@Component
public final class OnePlusFunction extends FunctionStruct {

	public static final SymbolStruct<?> ONE_PLUS = GlobalPackageStruct.COMMON_LISP.intern("1+").getSymbol();

	private static final long serialVersionUID = -5365841233828975994L;

	private OnePlusFunction() {
		super("Adds one to the object and returns the result.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		ONE_PLUS.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(ONE_PLUS);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> listArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("OBJECT").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(listArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		return onePlus(lispStructs[0]);
	}

	public NumberStruct onePlus(final LispStruct object) {
		if (object instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) object).getBigInteger();
			return new IntegerStruct(bigInteger.add(BigInteger.ONE));
		} else if (object instanceof FloatStruct) {
			final BigDecimal bigDecimal = ((FloatStruct) object).getBigDecimal();
			return new FloatStruct(bigDecimal.add(BigDecimal.ONE));
		} else if (object instanceof RatioStruct) {
			final BigFraction bigFraction = ((RatioStruct) object).getBigFraction();
			return new RatioStruct(bigFraction.add(BigFraction.ONE));
		} else {
			// TODO:
			throw new SimpleErrorException("One Plus only handles integers, floats, and ratios right now.");
		}
	}
}
