/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class GoSymbolElementGenerator extends GoElementGenerator<SymbolStruct<?>> {

	public GoSymbolElement generateGoElement(final SymbolStruct<?> tag) {
		return new GoSymbolElement(tag);
	}
}
