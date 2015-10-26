/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ScaleFloatFunction extends FunctionStruct {

	public static final SymbolStruct<?> SCALE_FLOAT = GlobalPackageStruct.COMMON_LISP.intern("SCALE-FLOAT").getSymbol();

	private static final long serialVersionUID = -7937903222796631877L;

	@Autowired
	private Printer printer;

	private ScaleFloatFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		SCALE_FLOAT.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(SCALE_FLOAT);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {
		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> floatSymbol = GlobalPackageStruct.COMMON_LISP.intern("FLOAT").getSymbol();
		final RequiredParameter requiredBinding1 = new RequiredParameter(floatSymbol);
		requiredBindings.add(requiredBinding1);

		final SymbolStruct<?> integerSymbol = GlobalPackageStruct.COMMON_LISP.intern("INTEGER").getSymbol();
		final RequiredParameter requiredBinding2 = new RequiredParameter(integerSymbol);
		requiredBindings.add(requiredBinding2);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct lispStruct1 = lispStructs[0];
		if (!(lispStruct1 instanceof FloatStruct)) {
			final String printedObject = printer.print(lispStruct1);
			throw new TypeErrorException("Argument not of type Float: " + printedObject);
		}
		final FloatStruct floatVal = (FloatStruct) lispStruct1;

		final LispStruct lispStruct2 = lispStructs[1];
		if (!(lispStruct2 instanceof IntegerStruct)) {
			final String printedObject = printer.print(lispStruct2);
			throw new TypeErrorException("Argument not of type Integer: " + printedObject);
		}
		final IntegerStruct integer = (IntegerStruct) lispStruct2;

		return floatVal.scaleFloat(integer);
	}
}
