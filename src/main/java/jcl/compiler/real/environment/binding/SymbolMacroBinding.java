/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class SymbolMacroBinding extends Binding {

	private static final long serialVersionUID = -7630096026388828215L;

	private final LispStruct expansion;

	public SymbolMacroBinding(final SymbolStruct<?> symbolStruct, final LispType type,
	                          final LispStruct expansion) {
		super(symbolStruct, type);
		this.expansion = expansion;
	}

	public LispStruct getExpansion() {
		return expansion;
	}
}
