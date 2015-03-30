/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.HashMap;
import java.util.Map;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

public class Closure {

	private final Closure parent;

	private final Map<SymbolStruct<?>, LispStruct> closureBindings;

	public Closure(final Closure parent, final Map<SymbolStruct<?>, LispStruct> closureBindings) {
		this.parent = parent;

		final Map<SymbolStruct<?>, LispStruct> allClosureBindings = new HashMap<>();
		if (parent != null) {
			allClosureBindings.putAll(parent.closureBindings);
		}
		allClosureBindings.putAll(closureBindings);
		this.closureBindings = allClosureBindings;
	}

	public Closure getParent() {
		return parent;
	}

	public Map<SymbolStruct<?>, LispStruct> getClosureBindings() {
		return closureBindings;
	}
}
