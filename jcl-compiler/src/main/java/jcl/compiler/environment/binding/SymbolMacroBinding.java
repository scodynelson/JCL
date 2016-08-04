/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.type.LispType;

public class SymbolMacroBinding extends Binding {

	private final LispStruct expansion;

	public SymbolMacroBinding(final SymbolStruct var, final LispType type,
	                          final LispStruct expansion) {
		super(var, type);
		this.expansion = expansion;
	}

	public LispStruct getExpansion() {
		return expansion;
	}
}
