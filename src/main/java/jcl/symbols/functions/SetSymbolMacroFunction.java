/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.functions.expanders.SymbolMacroExpanderImpl;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import jcl.types.SymbolType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolMacroFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetSymbolMacroFunction() {
		super("Creates a new symbol-macro with the provided expansion and sets the symbol-macro value of the provided symbol to it.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		final RequiredParameter symbolArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "SYMBOL").build();
		final RequiredParameter functionArg = RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "EXPANSION").build();
		return Arrays.asList(symbolArg, functionArg);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final SymbolStruct symbol =
				validator.validateType(lispStructs[0], functionName(), "Symbol", SymbolType.INSTANCE, SymbolStruct.class);
		final LispStruct expansion = lispStructs[1];

		final SymbolMacroExpander symbolMacroExpander = new SymbolMacroExpanderImpl(expansion);
		symbol.setSymbolMacroExpander(symbolMacroExpander);
		return symbol;
	}

	@Override
	protected String functionName() {
		return "SET-SYMBOL-MACRO";
	}
}
