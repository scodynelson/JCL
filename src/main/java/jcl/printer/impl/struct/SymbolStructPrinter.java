/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.printer.impl.SymbolPrinter;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SymbolStructPrinter extends SymbolPrinter<SymbolStruct<?>> {

	private static final long serialVersionUID = 5098070113503702856L;

	@Override
	protected String getName(final SymbolStruct<?> object) {
		return object.getName();
	}
}
