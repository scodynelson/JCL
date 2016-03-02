/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class UnbindSymbolFunctionFunction extends AbstractSystemFunctionStruct {

	// TODO: Get rid of this??
	public static final SymbolStruct UNBIND_SYMBOL_FUNCTION = GlobalPackageStruct.SYSTEM.intern("UNBIND-SYMBOL-FUNCTION").getSymbol();

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public UnbindSymbolFunctionFunction() {
		super("Unbinds the function value of the provided symbol from its current value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "SYMBOL").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol =
				validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);
		return symbol.unbindFunction();
	}

	@Override
	protected String functionName() {
		return "UNBIND-SYMBOL-FUNCTION";
	}
}
