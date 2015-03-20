/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class BlockStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -115779602179582479L;

	private final SymbolStruct<?> name;

	private final PrognStruct forms;

	public BlockStruct(final SymbolStruct<?> name, final PrognStruct forms) {
		this.name = name;
		this.forms = forms;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public PrognStruct getForms() {
		return forms;
	}
}
