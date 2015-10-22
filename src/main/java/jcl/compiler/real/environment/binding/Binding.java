/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import java.io.Serializable;

import jcl.LispType;
import jcl.symbols.SymbolStruct;

public class Binding implements Serializable {

	private static final long serialVersionUID = 5776546981120800982L;

	private final SymbolStruct<?> symbolStruct;

	private final LispType type;

	public Binding(final SymbolStruct<?> symbolStruct, final LispType type) {
		this.symbolStruct = symbolStruct;
		this.type = type;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public LispType getType() {
		return type;
	}
}
