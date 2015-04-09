/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.net.URI;

import jcl.pathnames.PathnameStruct;
import jcl.printer.LispPrinter;
import jcl.printer.PrinterVariables;
import jcl.symbols.BooleanStruct;
import org.springframework.stereotype.Component;

@Component
public class PathnameStructPrinter implements LispPrinter<PathnameStruct> {

	private static final long serialVersionUID = 778383508578673692L;

	@Override
	public String print(final PathnameStruct object) {
		final BooleanStruct printEscape = PrinterVariables.PRINT_ESCAPE.getValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printEscape.booleanValue()) {
			stringBuilder.append("#P");
		}
		stringBuilder.append('"');

		final URI uri = object.getUri();
		stringBuilder.append(uri);

		stringBuilder.append('"');

		return stringBuilder.toString();
	}
}
