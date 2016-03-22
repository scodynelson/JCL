/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import jcl.LispStruct;
import jcl.functions.SystemBuiltInFunctionStruct;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.functions.expanders.SymbolMacroExpanderImpl;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolMacroFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SET-SYMBOL-MACRO";
	private static final String SYMBOL_ARGUMENT = "SYMBOL";
	private static final String EXPANSION_ARGUMENT = "EXPANSION";

	public SetSymbolMacroFunction() {
		super("Creates a new symbol-macro with the provided expansion and sets the symbol-macro value of the provided symbol to it.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYMBOL_ARGUMENT)
		                .requiredParameter(EXPANSION_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct symbol = arguments.getRequiredArgument(SYMBOL_ARGUMENT, SymbolStruct.class);
		final LispStruct expansion = arguments.getRequiredArgument(EXPANSION_ARGUMENT);

		final SymbolMacroExpander symbolMacroExpander = new SymbolMacroExpanderImpl(expansion);
		symbol.setSymbolMacroExpander(symbolMacroExpander);
		return symbol;
	}
}
