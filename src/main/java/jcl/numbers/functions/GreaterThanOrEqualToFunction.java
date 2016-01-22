/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GreaterThanOrEqualToFunction extends FunctionStruct {

	public static final SymbolStruct GREATER_THAN_OR_EQUAL_TO = GlobalPackageStruct.COMMON_LISP.intern(">=").getSymbol();

	private static final long serialVersionUID = 2427273733050273073L;

	@Autowired
	private Printer printer;

	private GreaterThanOrEqualToFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		GREATER_THAN_OR_EQUAL_TO.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(GREATER_THAN_OR_EQUAL_TO);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredParameter requiredBinding = new RequiredParameter(firstArgSymbol);
		final List<RequiredParameter> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct restArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REALS").getSymbol();
		final RestParameter restBinding = new RestParameter(restArgSymbol);

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .restBinding(restBinding)
		                         .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final RealStruct[] reals = getReals(lispStructs);
		return RealStruct.isGreaterThanOrEqualTo(reals) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	private RealStruct[] getReals(final LispStruct... lispStructs) {

		final RealStruct[] numbers = new RealStruct[lispStructs.length];
		for (int i = 0; i < lispStructs.length; i++) {
			final LispStruct lispStruct = lispStructs[i];
			if (lispStruct instanceof RealStruct) {
				numbers[i] = (RealStruct) lispStruct;
			} else {
				final String printedObject = printer.print(lispStruct);
				throw new TypeErrorException("Argument not of type Real: " + printedObject);
			}
		}
		return numbers;
	}
}
