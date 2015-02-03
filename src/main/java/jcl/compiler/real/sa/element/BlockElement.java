/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class BlockElement implements Element {

	private static final long serialVersionUID = -115779602179582479L;

	private final SymbolStruct<?> name;
	private final List<LispStruct> forms;

	public BlockElement(final SymbolStruct<?> name, final List<LispStruct> forms) {
		this.name = name;
		this.forms = forms;
	}
}