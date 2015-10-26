/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

import jcl.symbols.SymbolStruct;

public class SpecialDeclarationStruct implements DeclarationStruct {

	private static final long serialVersionUID = 7353657327204677544L;

	private final SymbolStruct<?> var;

	public SpecialDeclarationStruct(final SymbolStruct<?> var) {
		this.var = var;
	}

	public SymbolStruct<?> getVar() {
		return var;
	}
}
