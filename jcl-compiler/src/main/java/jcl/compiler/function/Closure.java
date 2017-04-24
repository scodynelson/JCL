/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function;

import java.util.LinkedHashMap;
import java.util.Map;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;

public class Closure {

	private final Closure parent;

	private final Map<SymbolStruct, LispStruct> symbolBindings = new LinkedHashMap<>();

	private final Map<SymbolStruct, FunctionStruct> functionBindings = new LinkedHashMap<>();

	public Closure(final Closure parent) {
		this.parent = parent;

		if (parent != null) {
			symbolBindings.putAll(parent.symbolBindings);
			functionBindings.putAll(parent.functionBindings);
		}
	}

	public Closure getParent() {
		return parent;
	}

	public Map<SymbolStruct, LispStruct> getSymbolBindings() {
		return symbolBindings;
	}

	public Map<SymbolStruct, FunctionStruct> getFunctionBindings() {
		return functionBindings;
	}
}
