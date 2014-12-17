/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;

public class ReturnFromElement implements Element {

	private final SymbolStruct<?> name;
	private final LispStruct result;

	public ReturnFromElement(final SymbolStruct<?> name) {
		this.name = name;
		result = NILStruct.INSTANCE;
	}

	public ReturnFromElement(final SymbolStruct<?> name, final LispStruct result) {
		this.name = name;
		this.result = result;
	}
}
