/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class GoSymbolStructGenerator extends GoStructGenerator<SymbolStruct<?>> {

	private static final long serialVersionUID = 1893845150668677901L;

	public GoSymbolStruct generateGoElement(final SymbolStruct<?> tag) {
		return new GoSymbolStruct(tag);
	}
}
