/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class ReturnFromStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -6095397540807480492L;

	private final SymbolStruct<?> name;

	private final LispStruct result;

	public ReturnFromStruct(final SymbolStruct<?> name, final LispStruct result) {
		this.name = name;
		this.result = result;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public LispStruct getResult() {
		return result;
	}
}
