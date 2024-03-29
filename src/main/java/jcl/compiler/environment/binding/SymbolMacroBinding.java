/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import lombok.Getter;

@Getter
public class SymbolMacroBinding extends Binding {

	private final LispStruct expansion;

	public SymbolMacroBinding(final SymbolStruct var, final LispStruct expansion) {
		super(var);
		this.expansion = expansion;
	}
}
