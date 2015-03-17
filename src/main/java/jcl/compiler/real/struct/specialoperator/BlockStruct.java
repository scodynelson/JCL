/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class BlockStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -115779602179582479L;

	private final SymbolStruct<?> name;

	private final List<LispStruct> forms;

	public BlockStruct(final SymbolStruct<?> name, final List<LispStruct> forms) {
		this.name = name;
		this.forms = forms;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public List<LispStruct> getForms() {
		return forms;
	}
}
