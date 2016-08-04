/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.symbol;

import jcl.compiler.function.expanders.SymbolMacroExpander;
import jcl.compiler.function.expanders.SymbolMacroExpanderImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.SystemBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
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
