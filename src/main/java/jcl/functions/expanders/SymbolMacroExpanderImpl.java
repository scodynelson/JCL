/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

public class SymbolMacroExpanderImpl extends SymbolMacroExpander<LispStruct> {

	private final LispStruct expansion;

	public SymbolMacroExpanderImpl(final LispStruct expansion) {
		this.expansion = expansion;
	}

	@Override
	public LispStruct expand(final SymbolStruct<?> form, final Environment environment) {
		return expansion;
	}
}
