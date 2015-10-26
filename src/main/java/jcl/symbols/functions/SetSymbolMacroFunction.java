/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.symbols.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.functions.expanders.SymbolMacroExpanderImpl;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class SetSymbolMacroFunction extends FunctionStruct {

	public static final SymbolStruct<?> SET_SYMBOL_MACRO = GlobalPackageStruct.SYSTEM.intern("SET-SYMBOL-MACRO").getSymbol();

	private static final long serialVersionUID = -7741392752973190476L;

	private SetSymbolMacroFunction() {
		super("Creates a new symbol-macro with the provided expansion and sets the symbol-macro value of the provided symbol to it.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		SET_SYMBOL_MACRO.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(SET_SYMBOL_MACRO);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>(2);

		final SymbolStruct<?> symbolArgSymbol = GlobalPackageStruct.SYSTEM.intern("SYM").getSymbol();
		final RequiredParameter symbolArgRequiredBinding = new RequiredParameter(symbolArgSymbol);
		requiredBindings.add(symbolArgRequiredBinding);

		final SymbolStruct<?> expansionArgSymbol = GlobalPackageStruct.SYSTEM.intern("EXPANSION").getSymbol();
		final RequiredParameter expansionArgRequiredBinding = new RequiredParameter(expansionArgSymbol);
		requiredBindings.add(expansionArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> symbol = (SymbolStruct) lispStructs[0];
		final LispStruct expansion = lispStructs[1];

		final SymbolMacroExpander<LispStruct> symbolMacroExpander = new SymbolMacroExpanderImpl(expansion);
		return setSymbolMacro(symbol, symbolMacroExpander);
	}

	public LispStruct setSymbolMacro(final SymbolStruct<?> symbol, final SymbolMacroExpander<LispStruct> symbolMacroExpander) {
		symbol.setSymbolMacroExpander(symbolMacroExpander);
		return symbol;
	}
}
