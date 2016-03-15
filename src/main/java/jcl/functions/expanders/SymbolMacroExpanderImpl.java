/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.symbols.SymbolStruct;

public class SymbolMacroExpanderImpl extends SymbolMacroExpander {

	private final LispStruct expansion;

	public SymbolMacroExpanderImpl(final LispStruct expansion) {
		// TODO: Documentation???
		super("");
		this.expansion = expansion;
	}

	@Override
	public LispStruct expand(final SymbolStruct form, final Environment environment) {
		return expansion;
	}
}
