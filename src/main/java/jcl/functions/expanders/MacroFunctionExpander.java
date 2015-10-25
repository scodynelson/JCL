/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.functions.FunctionParameterBinding;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> {

	private static final long serialVersionUID = -4041262906159677088L;

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct<?> functionSymbol = getFunctionSymbol();
		functionSymbol.setMacroFunctionExpander(this);
	}

	@Override
	protected List<FunctionParameterBinding> getFunctionBindings(final LispStruct[] lispStructs) {
		// TODO: Override this to set "Whole" and "Environment" Parameters???
		return super.getFunctionBindings(lispStructs);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		// TODO: I don't think the first argument matters right now... This may just be the 'whole' parameter.
		// TODO: The 'environment' to be passed is the 'environment' parameter.
		// TODO: Do we do the '(let ((*environment* environment)) (... expansion-body ...))' here?
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(listStruct, environment);
	}
}
