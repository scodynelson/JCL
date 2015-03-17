/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

public class SymbolMacroElement extends SpecialOperatorStruct {

	private static final long serialVersionUID = -1618487345643376983L;

	private final SymbolStruct<?> symbolStruct;

	private final LispStruct form;

	public SymbolMacroElement(final SymbolStruct<?> symbolStruct, final LispStruct form) {
		this.symbolStruct = symbolStruct;
		this.form = form;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public LispStruct getForm() {
		return form;
	}
}
