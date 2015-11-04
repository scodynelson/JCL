/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import jcl.symbols.SymbolVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GensymFunction extends FunctionStruct {

	public static final SymbolStruct<?> GENSYM = GlobalPackageStruct.COMMON_LISP.intern("GENSYM").getSymbol();

	private static final long serialVersionUID = -1852620624613550769L;

	@Autowired
	private Printer printer;

	private GensymFunction() {
		super("Creates and returns a fresh, uninterned symbol.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		GENSYM.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(GENSYM);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final SymbolStruct<?> defaulting = GlobalPackageStruct.COMMON_LISP.intern("X").getSymbol();

		final SymbolStruct<?> defaultingSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("OUTPUT-FILE-P-" + System.nanoTime()).getSymbol();
		final SuppliedPParameter defaultingSuppliedPBinding = new SuppliedPParameter(defaultingSuppliedP);

		final OptionalParameter optionalBinding = new OptionalParameter(defaulting, NullStruct.INSTANCE, defaultingSuppliedPBinding);
		final List<OptionalParameter> optionalBindings = Collections.singletonList(optionalBinding);

		return new OrdinaryLambdaList.Builder().optionalBindings(optionalBindings)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		String gensymPrefix = "G";
		IntegerStruct gensymPostfix = SymbolVariables.GENSYM_COUNTER.getValue();
		if (lispStructs.length == 1) {
			final LispStruct defaulting = lispStructs[0];
			if (defaulting instanceof StringStruct) {
				gensymPrefix = ((StringStruct) defaulting).getAsJavaString();
				SymbolVariables.GENSYM_COUNTER.setValue((IntegerStruct) gensymPostfix.add(IntegerStruct.ONE));
			} else if (defaulting instanceof IntegerStruct) {
				gensymPostfix = (IntegerStruct) defaulting;
			} else {
				final String printedObject = printer.print(defaulting);
				throw new TypeErrorException("Expected either a String or Integer for GENSYM defaulting behavior. Got: " + printedObject);
			}
		} else {
			SymbolVariables.GENSYM_COUNTER.setValue((IntegerStruct) gensymPostfix.add(IntegerStruct.ONE));
		}

		final String symbolName = gensymPrefix + gensymPostfix.getBigInteger();
		return new SymbolStruct<>(symbolName);
	}
}
