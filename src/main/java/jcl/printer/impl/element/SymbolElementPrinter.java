/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.SymbolElement;
import jcl.printer.impl.SymbolPrinter;
import org.springframework.stereotype.Component;

@Component
public class SymbolElementPrinter extends SymbolPrinter<SymbolElement> {

	@Override
	protected String getName(final SymbolElement object) {
		return object.getSymbolName();
	}
}
