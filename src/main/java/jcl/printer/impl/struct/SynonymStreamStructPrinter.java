/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.printer.Printer;
import jcl.printer.impl.LispPrinter;
import jcl.streams.StreamStruct;
import jcl.streams.SynonymStreamStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SynonymStreamStructPrinter implements LispPrinter<SynonymStreamStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final SynonymStreamStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final SymbolStruct<StreamStruct> symbol = object.getSymbol();
		final String printedSymbol = printer.print(symbol);

		return "#<" + typeClassName + " to " + printedSymbol + '>';
	}
}
