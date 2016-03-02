/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class GoSymbolStructFactory extends GoStructFactory<SymbolStruct> {

	@Override
	public GoSymbolStruct getGoElement(final SymbolStruct tag) {
		return new GoSymbolStruct(tag);
	}
}
