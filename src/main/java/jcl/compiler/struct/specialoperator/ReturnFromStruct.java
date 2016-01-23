/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;

public class ReturnFromStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct name;

	private final LispStruct result;

	public ReturnFromStruct(final SymbolStruct name, final LispStruct result) {
		this.name = name;
		this.result = result;
	}

	public SymbolStruct getName() {
		return name;
	}

	public LispStruct getResult() {
		return result;
	}
}
