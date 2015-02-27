/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.compiler.real.element.SymbolElement;
import org.springframework.stereotype.Component;

@Component
public class GoSymbolElementGenerator extends GoElementGenerator<SymbolElement> {

	public GoSymbolElement generateGoElement(final SymbolElement tag) {
		return new GoSymbolElement(tag);
	}
}
