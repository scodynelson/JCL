/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.BooleanType;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CopySymbolFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public CopySymbolFunction() {
		super("Returns a fresh, uninterned symbol, the name of which is equal to and possibly the same as the name of the given symbol.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SYMBOL").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "COPY-PROPERTIES")
		                        .initForm(NILStruct.INSTANCE)
		                        .suppliedPBinding()
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol
				= validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);

		boolean copyProperties = false;
		if (lispStructs.length > 1) {
			final BooleanStruct shouldCopy
					= validator.validateType(lispStructs[1], functionName(), "Copy-Properties", BooleanType.INSTANCE, BooleanStruct.class);

			copyProperties = shouldCopy.booleanValue();
		}

		return symbol.copySymbol(copyProperties);
	}

	@Override
	protected String functionName() {
		return "COPY-SYMBOL";
	}
}
