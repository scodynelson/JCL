/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.functions;

import java.math.BigDecimal;
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
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class FloatFunction extends FunctionStruct {

	public static final SymbolStruct<?> FLOAT = GlobalPackageStruct.COMMON_LISP.intern("FLOAT").getSymbol();

	private static final long serialVersionUID = -1487899103566810524L;

	@Autowired
	private Printer printer;

	private FloatFunction() {
		super("", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		FLOAT.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(FLOAT);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> firstArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("REAL").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(firstArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> optionalArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("PROTOTYPE").getSymbol();

		final SymbolStruct<?> optionalSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("PROTOTYPE-P-" + System.nanoTime()).getSymbol();
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
		if (!(lispStruct instanceof RealStruct)) {
			final String printedObject = printer.print(lispStruct);
			throw new TypeErrorException("Argument not of type Real: " + printedObject);
		}
		final RealStruct real = (RealStruct) lispStruct;

		if (real instanceof FloatStruct) {
			return real;
		}

		final BigDecimal bigDecimal = real.bigDecimalValue();
		return new FloatStruct(bigDecimal);
	}
}
