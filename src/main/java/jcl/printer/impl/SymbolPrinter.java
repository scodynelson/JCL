/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.PrinterVariables;
import jcl.symbols.BooleanStruct;

public abstract class SymbolPrinter<O> implements LispPrinter<O> {

	private static final long serialVersionUID = 3265320493330001842L;

	@Override
	public String print(final O object) {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getValue();

		// TODO: deal with *PRINT-CASE* and *PRINT-ESCAPE*
		return getName(object);
	}

	protected abstract String getName(O object);
}
