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
import jcl.symbols.SymbolStruct;
import jcl.symbols.SymbolVariables;
import jcl.types.IntegerType;
import jcl.types.StringType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GensymFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6888123105525048725L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public GensymFunction() {
		super("Creates and returns a fresh, uninterned symbol.");
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return new OptionalParameter.Builder(GlobalPackageStruct.COMMON_LISP, "PREFIX")
				.suppliedPBinding()
				.buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		String gensymPrefix = "G";
		IntegerStruct gensymPostfix = SymbolVariables.GENSYM_COUNTER.getValue();
		if (lispStructs.length == 1) {
			final LispStruct defaulting = lispStructs[0];
			validator.validateTypes(defaulting, functionName(), "Prefix", StringType.INSTANCE, IntegerType.INSTANCE);

			if (defaulting instanceof StringStruct) {
				gensymPrefix = ((StringStruct) defaulting).getAsJavaString();
				SymbolVariables.GENSYM_COUNTER.setValue((IntegerStruct) gensymPostfix.add(IntegerStruct.ONE));
			} else if (defaulting instanceof IntegerStruct) {
				gensymPostfix = (IntegerStruct) defaulting;
			} else {
				throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
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
