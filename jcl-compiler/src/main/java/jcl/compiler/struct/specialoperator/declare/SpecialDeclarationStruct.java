/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.declare;

import jcl.lang.SymbolStructImpl;

public class SpecialDeclarationStruct implements DeclarationStruct {

	private final SymbolStructImpl var;

	public SpecialDeclarationStruct(final SymbolStructImpl var) {
		this.var = var;
	}

	public SymbolStructImpl getVar() {
		return var;
	}
}
