/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.SymbolStruct;
import jcl.symbols.SymbolVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GensymFunction extends AbstractCommonLispFunctionStruct {

	private static final long serialVersionUID = -1852620624613550769L;

	@Autowired
	private Printer printer;

	public GensymFunction() {
		super("Creates and returns a fresh, uninterned symbol.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "X").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

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

	@Override
	protected String functionName() {
		return "GENSYM";
	}
}
