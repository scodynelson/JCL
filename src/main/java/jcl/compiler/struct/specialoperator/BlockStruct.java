/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class BlockStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct name;

	private final PrognStruct forms;

	public BlockStruct(final SymbolStruct name, final PrognStruct forms) {
		this.name = name;
		this.forms = forms;
	}

	public SymbolStruct getName() {
		return name;
	}

	public PrognStruct getForms() {
		return forms;
	}
}
