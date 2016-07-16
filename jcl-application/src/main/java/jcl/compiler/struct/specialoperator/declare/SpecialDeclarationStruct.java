/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

import jcl.lang.SymbolStruct;

public class SpecialDeclarationStruct implements DeclarationStruct {

	private final SymbolStruct var;

	public SpecialDeclarationStruct(final SymbolStruct var) {
		this.var = var;
	}

	public SymbolStruct getVar() {
		return var;
	}
}
