/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.compiler.environment.Environment;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

public class SymbolMacroExpanderImpl extends SymbolMacroExpander {

	private final LispStruct expansion;

	public SymbolMacroExpanderImpl(final LispStruct expansion) {
		// TODO: Documentation???
		super("");
		this.expansion = expansion;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final SymbolStruct symbolStruct = (SymbolStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(symbolStruct, environment);
	}

	@Override
	public LispStruct expand(final SymbolStruct form, final Environment environment) {
		return expansion;
	}
}
